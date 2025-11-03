      ******************************************************************
      ******************************************************************
      ******************************************************************
      * NOTE: WHEN COMPARING A15 TO S35, BYANRP WILL NOT MATCH DUE TO
      *       A15 WORKING STORAGE TOTAL FIELDS BEING TOO SHORT SO IT
      *       TRUNCATES THE LEADING NUMBERS!!!
      ******************************************************************
      ******************************************************************
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BYANRP.
      *****************************************************************
      *                      (LP)
      *    DESCRIPTION:  BRANCH ANALYSIS REPORT
      *                      (BYFILE)
      *
      * REV:
      *  JFF 100295 ADDED CLASS SCAN FOR LOAN CLASS
      *  MJD 111195 ADDED MODIFICATIONS TO MAKE IT RUN IN A60.
      *             ADDED MISSING TYPES TO PERIOD TABLE.
      *             ADDED O2-COL AND 03-07REB TO WK-REC.
      *             COMBINED OT2COL W/ OTHCOL FOR DETAIL LINE.
      *             ADDED PRINT OF O3REB.
      * MJD 111795  CHANGED WK-REC TO MATCH BY DELINQ TABLE
      * JFF 121395  ADDED "IF LOCAL-PRINT-FG NOT = Y" LOGIC TO
      *             OPEN-PR-FILE IN INITIALIZATION.
      *  GLF 022398 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  MJD 031298 EXPANDED WK-SUMWGTTM AND WK-SUMWGTTA
      *             EXPANDED WK-NO-REPO
      *  GLF 032598 ADD REPO OPTION AND PAGE.
      *  JTG 040598 CHANGED VERSION TO 'B' RE: GLF 032598 CHANGE
      *  GLF 042098 1U WAS NOT WORKING FORM REPO FIELD, I FIXED IT
      *  BLV 043098 FIXED TO WORK WITH LOCAL SPOOLING BY GROUP
      *  MJD 051298 CHANGED TO INCLUDE BY-NO(10-11?) AND BY-AMT(10-11)
      *             TO GET "RB" AND "RO" STATS.
      *  MJD 051998 REMOVED XY-PASTDUE. CHANGED XY-EXREB TO BE XY-PART-CHGOFF
      *  MJD 980713 AT END OF GROUP, MOVE EXT-CONAME-RPT-LINES TO LN-COUNT NOT LN-WK.
      *  090498 BLV FIXED LOGIC FOR LOCAL SPOOLING BY GROUP (BREXEC)
      *  MJD 990823 FIXED BUG. IF FIRST BRANCHS' STATS ARE NOT THERE, THEN
      *             THE SECOND BRANCHS' STATS GET ADDED TWICE.
      *  BLV 990902 NOTE - THE BUG FIXED ABOVE MAY OCCUR WITH GROUP RANGE OR
      *             BRANCH RANGE - WILL STILL INVOLVE BRANCH ADDED TWICE!!
      *  MJD 990824 CLEANED UP WORKER## LOGIC. ADDED OPTION TO PRINT
      *             REPO SUBTOTALS BY CLASS WITH OUTSTANDING.
      *  BLV 990902 FIX RANGE PRINT IF GROUP SELECTED
      *  MJD 990917 EXPANDED REPO SUBTOTALS ON PAGE 1 TO BREAK DOWN
      *             BY TOTLN. TOTSF, AND GRAND.
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BAH 001113 FIXED ZERO-TOTALS TO PERFORM ZERO-P6-TOTALS, IT WAS
      *             PERFORMING ZERO-S6-TOTALS TWICE, OOPS !! PL# 302
      *  MJD 001116 RE REPO TOTALS: I REMOVED ALL UNNECCESSARY TESTS
      *             FOR REPO REDEEMED. REPO REDEEMED GAIN/LOSS WILL NOT
      *             BE INCLUDED ANYWHERE ON THIS REPORT. (PL#0303 LIBERAL)
      *  BLV 021125 SUBTRACT P2'S FROM P&L ACCUMULATIONS FOR WORLD -
      *             THEY'RE USING P2'S FOR NON-FILE PMTS
      *  BAH 040202 ADDED ENDING CLASS RANGE FOR WORLD STEWART CONVERSION
      *             WORLD PR# 341
      *  BAH 040603 FORCE LC-CODE 1, CLASS 1, TO NOT BE SALESFIN SO THEY
      *             ADD INTO THE LOAN TOTALS, PACESETTER PR# 2298
      *  BLF 050303 FIX P&L ACCUMULATION TO NOT ADD INACTIVE P&L'S IN
      *             TO VOLUME, SUN PL# 469
      *  BLF 050303 FIX ROUTINE, IF I-AM-WORLD, SUBTRACT OUT P2'S FROM
      *             P&L ACCUMULATION, SUBSCRIPT NOT SET, WAS GIVING
      *             BOGUS NUMBERS, PL#469
      *  BLF 050331 FIX VOLUME PERCENT CALCULATIONS.  WAS ADDING
      *             PERCENTS FOR CLASS TOTALS, NOT PRINTING PERCENTS
      *             FOR BRANCH AND GRAND TOTALS.  COMPUTE AT EACH
      *             LEVEL NOW.  UNION, PL#475
      *  BAH 090122 EXPANDED D1-2-OUTSTAND 1 BYTE FOR WORLD
      *              SHIFTED TAGS OVER 1 SPACE
      *  BAH 090206 STAGGERED CORPORATE TOTALS FOR WORLD
      *
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *
      *  BAH 170127 REMOVED A "GO TO" FROM REPO-HEADING SECTION THAT TOOK
      *             YOU OUT OF THE SECTION! BEEN IN HERE SINCE AT LEAST
      *             1999! FOUND WHEN TESTING A30, PL#962
      *  BAH 180411 CHANGED D1-2-CLASS TO PIC 99 FROM PIC XX
      *             REMOVED LOCAL-PRINT-FG, WAS NOT ENTERED BUT WAS STILL IN
      *             EOBUF, THEREFORE, WHATEVER WAS STORED IN THAT EOBUF 
      *             POSITION FROM THE LAST PROGRAM RAN, WAS SITTING IN THIS
      *             FLAG VALUE. MESSED UP DATA PATH FOR OPENING PRINTER, GOT
      *             AN OPEN STAT 30.
      *   CS 180416 THE CORPORATE TOTALS PAGE WAS JUNK. WE UPDATE A WORK
      *             FILE WITH TOTALS BUT GRAND TOTALS MOVE THE TABLES
      *             FROM THE WORK FILE TO THE CORRESPONDING TABLES IN 
      *             BY-FILE. BY TABLES HAD COMP3'S AND THE WORK TABLES
      *             WERE COMP.
      *
      * KEC 2018.0423 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      *
      * BAH 181026 GLOBAL BYFILE
      * KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      *            WK-REC WAS A HARDCODED COPY OF THE BY-REC WHICH
      *            APPEARS TO NOT BEEN UPDATED OVER TIME FOR NEW FIELDS
      *            SO I HAVE USED LP01BYA_AREA.CPY & LP01BYB_AREA.CPY
      *            WITH REPLACING LEADING ON THEM TO RENAME THEM TO WK.
      * BAH 2019.0415 CHANGED D3-4-CLASS AND D5-CLASS TO PIC 99 FROM XX.
      *               CAUSING CLASS TO TRUNCATE #QA0001
      * BAH 2019.0509 PIGGYBACK PRINT NOT WORKING, OPEN-PR MUST BE AFTER
      *               ANY "IN PROGRESS" WRFORM MESSAGE, QA0021
      *  BAH 20190924 REMOVED ACCESS-CALL BYAFILE, BYB
      *  RMZ 20230808 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
      /
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".
      /*****************************************************
      *   SUMMARY TOTALS WORK RECORD                       *
      ******************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-LOAN-TYPE           PIC 9(2).
               05  WK-CLASS               PIC 9(3).
       COPY "LIBLP/LP01BYA_AREA.CPY" REPLACING LEADING "BYA-" BY "WK-".
       COPY "LIBLP/LP01BYB_AREA.CPY" REPLACING LEADING "BYB-" BY "WK-".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/BYANRP.CBL S35 08/08/24-09:32:07 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-ACCT-NOBEG-IC           PIC 99.            
       01  QWS-END-IC           PIC 99.
       01  QWS-BEG-ST           PIC X(2).
       01  QWS-END-ST           PIC X(2).
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK-IFN               PIC X(2)     VALUE "WK".

       01  BY1-CONN-OPEN-SW     PIC X        VALUE "N".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999   COMP   VALUE 0.
           03  LN-COUNT         PIC 999   COMP   VALUE 99.
           03  LN-WK            PIC 999   COMP   VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)  COMP   VALUE 0.
      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB1                 PIC 9(2)  COMP   VALUE ZERO.
       01  SUB2                 PIC 9(2)  COMP   VALUE ZERO.
       01  SUB3                 PIC 9(2)  COMP   VALUE ZERO.
       01  S1                   PIC 9(2)  COMP   VALUE ZERO.
       01  S2                   PIC 9(2)  COMP   VALUE ZERO.
       01  S3                   PIC 9(2)  COMP   VALUE ZERO.
       01  S4                   PIC 9(2)  COMP   VALUE ZERO.
       01  K                    PIC 9(2)  COMP   VALUE ZERO.

       01  E-O-FLAG             PIC X VALUE SPACE.
           88  E-O-F            VALUE "X".
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  SAVE-BEG-BR          PIC 9(4) VALUE 0.
       01  HOLD-BRANCH          PIC 9(4) VALUE ZERO.
       01  HOLD-WK-TYPE         PIC 9(2) VALUE 0.

       01  MAX-PARTS-PER-SECTION PIC 9   VALUE 3.
       01  MAX-SECTIONS-PER-BR   PIC 9   VALUE 6.
       01  FIRST-PART            PIC 9   VALUE 1.
       01  FIRST-SECTION         PIC 9   VALUE 1.

       01  DISPLAY-DATE             PIC 9(4).
       01  DISPLAY-DATEX            REDEFINES DISPLAY-DATE.
           03  DISPLAY-MO           PIC 99.
           03  DISPLAY-YR           PIC 99.
       01  DATE-LIST                PIC X(5)  VALUE "MMYY.".
       01  HOLD-BY-DATE-CCYYMM      PIC 9(6).

       01  SOURCE-VALUES.
           03  FILLER           PIC X(33) VALUE
               "NB RF FB PB PBDPBIRN SC OT RB RO ".
       01  SOURCE-TABLE REDEFINES SOURCE-VALUES.
           03  SOURCE-ELE       PIC XXX OCCURS 11 TIMES.
      *
       01  REPO-VALUES.
           03  FILLER           PIC X(18) VALUE
               "ONHANDREDEEMSLD AR".
       01  REPO-TABLE REDEFINES REPO-VALUES.
           03  REPO-ELE       PIC X(6) OCCURS 3 TIMES.
      *
      *
       01  SECTION-SUB          PIC 99 VALUE 0.
           88  LOANS-OUT        VALUE 1.
           88  P-AND-L          VALUE 2.
           88  COLLECTIONS      VALUE 3.
           88  REBATES          VALUE 4.
           88  APPLICATIONS     VALUE 5.
           88  REPOS            VALUE 6.
           88  GRAND-TOTS       VALUE 7.

       01  CORPORATE-FLAG       PIC X VALUE SPACE.
           88  CORP-TOTALS      VALUE "X".
      *
       01  ACCT-TYPE            PIC 99   VALUE ZERO.
           88  SMALL-LOANS      VALUE 1.
           88  SALES-FINANCE    VALUE 2.
           88  SERVICE-ACCOUNTS VALUE 3.

       01  TAG-TABLE-VALUES.
           03  FILLER     PIC X(30) VALUE
               "TOTLN:TOTSF:TOTSA:  SUB:GRAND:".
       01  TAG-TABLE REDEFINES TAG-TABLE-VALUES.
           03  TAGNAME    PIC X(6) OCCURS 5 TIMES.
      **************************************************
      *      USED FOR DETAIL CALCULATIONS              *
      **************************************************
       01  SECTION1-WORKERS.
           03  OUTS-OUTS               PIC S9(9)V99 COMP VALUE 0.
           03  OUTS-PRIN-MTD           PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-PRIN-YTD           PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-PRIN-PCT           PIC S9(3)V99 COMP VALUE 0.
           03  OUTS-VOL-MTD            PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-VOL-YTD            PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-VOL-LYR            PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-VOL-PCT            PIC S9(3)V99 COMP VALUE 0.
           03  OUTS-ACCTS              PIC S9(6)    COMP VALUE 0.
           03  OUTS-ACCTS-MTD          PIC S9(5)    COMP VALUE 0.
           03  OUTS-ACCTS-YTD          PIC S9(6)    COMP VALUE 0.
           03  OUTS-NWACCTS-MTD        PIC S9(6)    COMP VALUE 0.
           03  OUTS-NWACCTS-YTD        PIC S9(6)    COMP VALUE 0.
           03  OUTS-NWACCTS-LYR        PIC S9(6)    COMP VALUE 0.

       01  SECTION2-WORKERS.
           03  PL-OUTS                 PIC S9(8)V99 COMP VALUE 0.
           03  PL-PRIN-MTD             PIC S9(8)V99 COMP VALUE 0.
           03  PL-PRIN-YTD             PIC S9(8)V99 COMP VALUE 0.
           03  PL-PRIN-PCT             PIC S9(3)V99 COMP VALUE 0.
           03  PL-VOL-MTD              PIC S9(8)V99 COMP VALUE 0.
           03  PL-VOL-YTD              PIC S9(8)V99 COMP VALUE 0.
           03  PL-VOL-LYR              PIC S9(8)V99 COMP VALUE 0.
           03  PL-VOL-PCT              PIC S9(3)V99 COMP VALUE 0.
           03  PL-ACCTS                PIC S9(6)    COMP VALUE 0.
           03  PL-ACCTS-MTD            PIC S9(5)    COMP VALUE 0.
           03  PL-ACCTS-YTD            PIC S9(6)    COMP VALUE 0.
           03  PL-NWACCTS-MTD          PIC S9(6)    COMP VALUE 0.
           03  PL-NWACCTS-YTD          PIC S9(6)    COMP VALUE 0.
           03  PL-NWACCTS-LYR          PIC S9(6)    COMP VALUE 0.

       01  TOT-PL-PERIOD-TBL   OCCURS 3.
           03  TOT-PL-ACTIVITY-TBL OCCURS 2.
               05  TOT-NO-PLA          PIC S9(10).
               05  TOT-AMT-PLA         PIC S9(10)V99.
               05  TOT-NO-PLI          PIC S9(10).
               05  TOT-AMT-PLI         PIC S9(10)V99.

       01  SECTION3-WORKERS.
           03  COL-OTH-MTD             PIC S9(8)V99 COMP VALUE 0.
           03  COL-OTH-YTD             PIC S9(8)V99 COMP VALUE 0.
           03  COL-INT-MTD             PIC S9(8)V99 COMP VALUE 0.
           03  COL-INT-YTD             PIC S9(8)V99 COMP VALUE 0.
           03  COL-LATE-MTD            PIC S9(8)V99 COMP VALUE 0.
           03  COL-LATE-YTD            PIC S9(8)V99 COMP VALUE 0.
           03  COL-PRIN-MTD            PIC S9(8)V99 COMP VALUE 0.
           03  COL-PRIN-YTD            PIC S9(8)V99 COMP VALUE 0.
           03  COL-DEF-MTD             PIC S9(8)V99 COMP VALUE 0.
           03  COL-DEF-YTD             PIC S9(8)V99 COMP VALUE 0.
           03  COL-PLA-MTD             PIC S9(8)V99 COMP VALUE 0.
           03  COL-PLA-YTD             PIC S9(8)V99 COMP VALUE 0.
           03  COL-PLI-MTD             PIC S9(8)V99 COMP VALUE 0.
           03  COL-PLI-YTD             PIC S9(8)V99 COMP VALUE 0.
           03  COL-RNP-MTD             PIC S9(8)V99 COMP VALUE 0.
           03  COL-RNP-YTD             PIC S9(8)V99 COMP VALUE 0.
           03  COL-TOT-MTD             PIC S9(8)V99 COMP VALUE 0.
           03  COL-TOT-YTD             PIC S9(8)V99 COMP VALUE 0.

       01  SECTION4-WORKERS.
           03  REB-INT-1               PIC S9(8)V99 COMP VALUE 0.
           03  REB-INT-2               PIC S9(8)V99 COMP VALUE 0.
           03  REB-CL-1                PIC S9(8)V99 COMP VALUE 0.
           03  REB-CL-2                PIC S9(8)V99 COMP VALUE 0.
           03  REB-AH-1                PIC S9(8)V99 COMP VALUE 0.
           03  REB-AH-2                PIC S9(8)V99 COMP VALUE 0.
           03  REB-PP-1                PIC S9(8)V99 COMP VALUE 0.
           03  REB-PP-2                PIC S9(8)V99 COMP VALUE 0.
           03  REB-O1-1                PIC S9(8)V99 COMP VALUE 0.
           03  REB-O1-2                PIC S9(8)V99 COMP VALUE 0.
           03  REB-O2-1                PIC S9(8)V99 COMP VALUE 0.
           03  REB-O2-2                PIC S9(8)V99 COMP VALUE 0.
           03  REB-O3-1                PIC S9(8)V99 COMP VALUE 0.
           03  REB-O3-2                PIC S9(8)V99 COMP VALUE 0.
           03  REB-SC-1                PIC S9(8)V99 COMP VALUE 0.
           03  REB-SC-2                PIC S9(8)V99 COMP VALUE 0.
           03  REB-MF-1                PIC S9(8)V99 COMP VALUE 0.
           03  REB-MF-2                PIC S9(8)V99 COMP VALUE 0.
           03  REB-DF-1                PIC S9(8)V99 COMP VALUE 0.
           03  REB-DF-2                PIC S9(8)V99 COMP VALUE 0.
           03  REB-TOT-1               PIC S9(8)V99 COMP VALUE 0.
           03  REB-TOT-2               PIC S9(8)V99 COMP VALUE 0.

       01  SECTION5-WORKERS.
           03  APPL-01                 PIC S9(7)    COMP VALUE 0.
           03  APPL-02                 PIC S9(7)    COMP VALUE 0.
           03  APPL-03                 PIC S9(7)    COMP VALUE 0.
           03  APPL-04                 PIC S9(7)    COMP VALUE 0.
           03  APPL-05                 PIC S9(7)    COMP VALUE 0.
           03  APPL-06                 PIC S9(7)    COMP VALUE 0.
           03  APPL-07                 PIC S9(7)    COMP VALUE 0.
           03  APPL-08                 PIC S9(3)V99 COMP VALUE 0.
           03  APPL-09                 PIC S9(7)    COMP VALUE 0.
           03  APPL-10                 PIC S9(3)V99 COMP VALUE 0.

       01  SECTION6-WORKERS.
           03  REPO-OUTS               PIC S9(8)V99 COMP VALUE 0.
           03  REPO-PRIN-MTD           PIC S9(8)V99 COMP VALUE 0.
           03  REPO-PRIN-YTD           PIC S9(8)V99 COMP VALUE 0.
           03  REPO-PRIN-PCT           PIC S9(3)V99 COMP VALUE 0.
           03  REPO-VOL-MTD            PIC S9(8)V99 COMP VALUE 0.
           03  REPO-VOL-YTD            PIC S9(8)V99 COMP VALUE 0.
           03  REPO-VOL-LYR            PIC S9(8)V99 COMP VALUE 0.
           03  REPO-VOL-PCT            PIC S9(3)V99 COMP VALUE 0.
           03  REPO-ACCTS              PIC S9(6)    COMP VALUE 0.
           03  REPO-ACCTS-MTD          PIC S9(5)    COMP VALUE 0.
           03  REPO-ACCTS-YTD          PIC S9(6)    COMP VALUE 0.
           03  REPO-NWACCTS-MTD        PIC S9(6)    COMP VALUE 0.
           03  REPO-NWACCTS-YTD        PIC S9(6)    COMP VALUE 0.
           03  REPO-NWACCTS-LYR        PIC S9(6)    COMP VALUE 0.

       01  SOURCE-TOTALS.
           03  SRC-OUTS                PIC S9(9)V99 COMP VALUE 0.
           03  SRC-PRIN-MTD            PIC S9(8)V99 COMP VALUE 0.
           03  SRC-PRIN-YTD            PIC S9(8)V99 COMP VALUE 0.
           03  SRC-PRIN-PCT            PIC S9(3)V99 COMP VALUE 0.
           03  SRC-VOL-MTD             PIC S9(8)V99 COMP VALUE 0.
           03  SRC-VOL-YTD             PIC S9(8)V99 COMP VALUE 0.
           03  SRC-VOL-LYR             PIC S9(8)V99 COMP VALUE 0.
           03  SRC-VOL-PCT             PIC S9(3)V99 COMP VALUE 0.
           03  SRC-ACCTS               PIC S9(6)    COMP VALUE 0.
           03  SRC-ACCTS-MTD           PIC S9(5)    COMP VALUE 0.
           03  SRC-ACCTS-YTD           PIC S9(6)    COMP VALUE 0.
           03  SRC-NWACCTS-MTD         PIC S9(6)    COMP VALUE 0.
           03  SRC-NWACCTS-YTD         PIC S9(6)    COMP VALUE 0.
           03  SRC-NWACCTS-LYR         PIC S9(6)    COMP VALUE 0.

       01  REPO-TOTALS.
           03  REPO-TOTOUTS            PIC S9(8)V99 COMP VALUE 0.
           03  REPO-TOTPRIN-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  REPO-TOTPRIN-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  REPO-TOTPRIN-PCT        PIC S9(3)V99 COMP VALUE 0.
           03  REPO-TOTVOL-MTD         PIC S9(8)V99 COMP VALUE 0.
           03  REPO-TOTVOL-YTD         PIC S9(8)V99 COMP VALUE 0.
           03  REPO-TOTVOL-LYR         PIC S9(8)V99 COMP VALUE 0.
           03  REPO-TOTVOL-PCT         PIC S9(3)V99 COMP VALUE 0.
           03  REPO-TOTACCTS           PIC S9(6)    COMP VALUE 0.
           03  REPO-TOTACCTS-MTD       PIC S9(5)    COMP VALUE 0.
           03  REPO-TOTACCTS-YTD       PIC S9(6)    COMP VALUE 0.
           03  REPO-TOTNWACCTS-MTD     PIC S9(6)    COMP VALUE 0.
           03  REPO-TOTNWACCTS-YTD     PIC S9(6)    COMP VALUE 0.
           03  REPO-TOTNWACCTS-LYR     PIC S9(6)    COMP VALUE 0.

       01  SUB-TOTALS.
           03  S-O-OUTS                PIC S9(9)V99 COMP VALUE 0.
           03  S-O-PRIN-MTD            PIC S9(8)V99 COMP VALUE 0.
           03  S-O-PRIN-YTD            PIC S9(8)V99 COMP VALUE 0.
           03  S-O-PRIN-PCT            PIC S9(3)V99 COMP VALUE 0.
           03  S-O-VOL-MTD             PIC S9(8)V99 COMP VALUE 0.
           03  S-O-VOL-YTD             PIC S9(8)V99 COMP VALUE 0.
           03  S-O-VOL-LYR             PIC S9(8)V99 COMP VALUE 0.
           03  S-O-VOL-PCT             PIC S9(3)V99 COMP VALUE 0.
           03  S-O-ACCTS               PIC S9(6)    COMP VALUE 0.
           03  S-O-ACCTS-MTD           PIC S9(5)    COMP VALUE 0.
           03  S-O-ACCTS-YTD           PIC S9(6)    COMP VALUE 0.
           03  S-O-NWACCTS-MTD         PIC S9(6)    COMP VALUE 0.
           03  S-O-NWACCTS-YTD         PIC S9(6)    COMP VALUE 0.
           03  S-O-NWACCTS-LYR         PIC S9(6)    COMP VALUE 0.
           03  S-R-OUTS                PIC S9(8)V99 COMP VALUE 0.
           03  S-R-PRIN-MTD            PIC S9(8)V99 COMP VALUE 0.
           03  S-R-PRIN-YTD            PIC S9(8)V99 COMP VALUE 0.
           03  S-R-PRIN-PCT            PIC S9(3)V99 COMP VALUE 0.
           03  S-R-VOL-MTD             PIC S9(8)V99 COMP VALUE 0.
           03  S-R-VOL-YTD             PIC S9(8)V99 COMP VALUE 0.
           03  S-R-VOL-LYR             PIC S9(8)V99 COMP VALUE 0.
           03  S-R-VOL-PCT             PIC S9(3)V99 COMP VALUE 0.
           03  S-R-ACCTS               PIC S9(6)    COMP VALUE 0.
           03  S-R-ACCTS-MTD           PIC S9(5)    COMP VALUE 0.
           03  S-R-ACCTS-YTD           PIC S9(6)    COMP VALUE 0.
           03  S-R-NWACCTS-MTD         PIC S9(6)    COMP VALUE 0.
           03  S-R-NWACCTS-YTD         PIC S9(6)    COMP VALUE 0.
           03  S-R-NWACCTS-LYR         PIC S9(6)    COMP VALUE 0.

      **************************************************
      *      USED FOR SECTION TOTAL CALCULATIONS       *
      **************************************************
       01  S1-TOTALS.
           03  OUTS-S-TOTOUTS          PIC S9(9)V99 COMP VALUE 0.
           03  OUTS-S-TOTPRIN-MTD      PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-S-TOTPRIN-YTD      PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-S-TOTPRIN-PCT      PIC S9(3)V99 COMP VALUE 0.
           03  OUTS-S-TOTVOL-MTD       PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-S-TOTVOL-YTD       PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-S-TOTVOL-LYR       PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-S-TOTVOL-PCT       PIC S9(3)V99 COMP VALUE 0.
           03  OUTS-S-TOTACCTS         PIC S9(6)    COMP VALUE 0.
           03  OUTS-S-TOTACCTS-MTD     PIC S9(5)    COMP VALUE 0.
           03  OUTS-S-TOTACCTS-YTD     PIC S9(6)    COMP VALUE 0.
           03  OUTS-S-TOTNWACCTS-MTD   PIC S9(6)    COMP VALUE 0.
           03  OUTS-S-TOTNWACCTS-YTD   PIC S9(6)    COMP VALUE 0.
           03  OUTS-S-TOTNWACCTS-LYR   PIC S9(6)    COMP VALUE 0.

       01  S2-TOTALS.
           03  PL-S-TOTOUTS            PIC S9(8)V99 COMP VALUE 0.
           03  PL-S-TOTPRIN-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  PL-S-TOTPRIN-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  PL-S-TOTPRIN-PCT        PIC S9(3)V99 COMP VALUE 0.
           03  PL-S-TOTVOL-MTD         PIC S9(8)V99 COMP VALUE 0.
           03  PL-S-TOTVOL-YTD         PIC S9(8)V99 COMP VALUE 0.
           03  PL-S-TOTVOL-LYR         PIC S9(8)V99 COMP VALUE 0.
           03  PL-S-TOTVOL-PCT         PIC S9(3)V99 COMP VALUE 0.
           03  PL-S-TOTACCTS           PIC S9(6)    COMP VALUE 0.
           03  PL-S-TOTACCTS-MTD       PIC S9(5)    COMP VALUE 0.
           03  PL-S-TOTACCTS-YTD       PIC S9(6)    COMP VALUE 0.
           03  PL-S-TOTNWACCTS-MTD     PIC S9(6)    COMP VALUE 0.
           03  PL-S-TOTNWACCTS-YTD     PIC S9(6)    COMP VALUE 0.
           03  PL-S-TOTNWACCTS-LYR     PIC S9(6)    COMP VALUE 0.

       01  S3-TOTALS.
           03  COL-S-TOTOTH-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTOTH-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTINT-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTINT-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTLATE-MTD       PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTLATE-YTD       PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTPRIN-MTD       PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTPRIN-YTD       PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTDEF-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTDEF-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTPLA-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTPLA-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTPLI-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTPLI-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTRNP-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTRNP-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTTOT-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-S-TOTTOT-YTD        PIC S9(8)V99 COMP VALUE 0.

       01  S4-TOTALS.
           03  REB-S-TOTINT-1          PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTINT-2          PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTCL-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTCL-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTAH-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTAH-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTPP-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTPP-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTO1-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTO1-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTO2-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTO2-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTO3-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTO3-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTSC-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTSC-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTMF-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTMF-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTDF-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTDF-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTTOT-1          PIC S9(8)V99 COMP VALUE 0.
           03  REB-S-TOTTOT-2          PIC S9(8)V99 COMP VALUE 0.

       01  S5-TOTALS.
           03  APPL-S-TOT01            PIC S9(7)    COMP VALUE 0.
           03  APPL-S-TOT02            PIC S9(7)    COMP VALUE 0.
           03  APPL-S-TOT03            PIC S9(7)    COMP VALUE 0.
           03  APPL-S-TOT04            PIC S9(7)    COMP VALUE 0.
           03  APPL-S-TOT05            PIC S9(7)    COMP VALUE 0.
           03  APPL-S-TOT06            PIC S9(7)    COMP VALUE 0.
           03  APPL-S-TOT07            PIC S9(7)    COMP VALUE 0.
           03  APPL-S-TOT08            PIC S9(3)V99 COMP VALUE 0.
           03  APPL-S-TOT09            PIC S9(7)    COMP VALUE 0.
           03  APPL-S-TOT10            PIC S9(3)V99 COMP VALUE 0.

       01  S6-TOTALS.
           03  REPO-S-TOTOUTS          PIC S9(8)V99 COMP VALUE 0.
           03  REPO-S-TOTPRIN-MTD      PIC S9(8)V99 COMP VALUE 0.
           03  REPO-S-TOTPRIN-YTD      PIC S9(8)V99 COMP VALUE 0.
           03  REPO-S-TOTPRIN-PCT      PIC S9(3)V99 COMP VALUE 0.
           03  REPO-S-TOTVOL-MTD       PIC S9(8)V99 COMP VALUE 0.
           03  REPO-S-TOTVOL-YTD       PIC S9(8)V99 COMP VALUE 0.
           03  REPO-S-TOTVOL-LYR       PIC S9(8)V99 COMP VALUE 0.
           03  REPO-S-TOTVOL-PCT       PIC S9(3)V99 COMP VALUE 0.
           03  REPO-S-TOTACCTS         PIC S9(6)    COMP VALUE 0.
           03  REPO-S-TOTACCTS-MTD     PIC S9(5)    COMP VALUE 0.
           03  REPO-S-TOTACCTS-YTD     PIC S9(6)    COMP VALUE 0.
           03  REPO-S-TOTNWACCTS-MTD   PIC S9(6)    COMP VALUE 0.
           03  REPO-S-TOTNWACCTS-YTD   PIC S9(6)    COMP VALUE 0.
           03  REPO-S-TOTNWACCTS-LYR   PIC S9(6)    COMP VALUE 0.
      /
      **************************************************
      *     USED FOR PART TOTALS WITHIN SECTIONS       *
      **************************************************
       01  P1-TOTALS.
           03  OUTS-P-TOTOUTS          PIC S9(9)V99 COMP VALUE 0.
           03  OUTS-P-TOTPRIN-MTD      PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-P-TOTPRIN-YTD      PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-P-TOTPRIN-PCT      PIC S9(3)V99 COMP VALUE 0.
           03  OUTS-P-TOTVOL-MTD       PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-P-TOTVOL-YTD       PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-P-TOTVOL-LYR       PIC S9(8)V99 COMP VALUE 0.
           03  OUTS-P-TOTVOL-PCT       PIC S9(3)V99 COMP VALUE 0.
           03  OUTS-P-TOTACCTS         PIC S9(6)    COMP VALUE 0.
           03  OUTS-P-TOTACCTS-MTD     PIC S9(5)    COMP VALUE 0.
           03  OUTS-P-TOTACCTS-YTD     PIC S9(6)    COMP VALUE 0.
           03  OUTS-P-TOTNWACCTS-MTD   PIC S9(6)    COMP VALUE 0.
           03  OUTS-P-TOTNWACCTS-YTD   PIC S9(6)    COMP VALUE 0.
           03  OUTS-P-TOTNWACCTS-LYR   PIC S9(6)    COMP VALUE 0.

       01  P2-TOTALS.
           03  PL-P-TOTOUTS            PIC S9(8)V99 COMP VALUE 0.
           03  PL-P-TOTPRIN-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  PL-P-TOTPRIN-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  PL-P-TOTPRIN-PCT        PIC S9(3)V99 COMP VALUE 0.
           03  PL-P-TOTVOL-MTD         PIC S9(8)V99 COMP VALUE 0.
           03  PL-P-TOTVOL-YTD         PIC S9(8)V99 COMP VALUE 0.
           03  PL-P-TOTVOL-LYR         PIC S9(8)V99 COMP VALUE 0.
           03  PL-P-TOTVOL-PCT         PIC S9(3)V99 COMP VALUE 0.
           03  PL-P-TOTACCTS           PIC S9(6)    COMP VALUE 0.
           03  PL-P-TOTACCTS-MTD       PIC S9(5)    COMP VALUE 0.
           03  PL-P-TOTACCTS-YTD       PIC S9(6)    COMP VALUE 0.
           03  PL-P-TOTNWACCTS-MTD     PIC S9(6)    COMP VALUE 0.
           03  PL-P-TOTNWACCTS-YTD     PIC S9(6)    COMP VALUE 0.
           03  PL-P-TOTNWACCTS-LYR     PIC S9(6)    COMP VALUE 0.

       01  P3-TOTALS.
           03  COL-P-TOTOTH-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTOTH-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTINT-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTINT-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTLATE-MTD       PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTLATE-YTD       PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTPRIN-MTD       PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTPRIN-YTD       PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTDEF-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTDEF-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTPLA-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTPLA-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTPLI-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTPLI-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTRNP-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTRNP-YTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTTOT-MTD        PIC S9(8)V99 COMP VALUE 0.
           03  COL-P-TOTTOT-YTD        PIC S9(8)V99 COMP VALUE 0.

       01  P4-TOTALS.
           03  REB-P-TOTINT-1          PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTINT-2          PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTCL-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTCL-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTAH-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTAH-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTPP-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTPP-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTO1-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTO1-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTO2-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTO2-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTO3-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTO3-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTSC-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTSC-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTMF-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTMF-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTDF-1           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTDF-2           PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTTOT-1          PIC S9(8)V99 COMP VALUE 0.
           03  REB-P-TOTTOT-2          PIC S9(8)V99 COMP VALUE 0.

       01  P5-TOTALS.
           03  APPL-P-TOT01            PIC S9(7)    COMP VALUE 0.
           03  APPL-P-TOT02            PIC S9(7)    COMP VALUE 0.
           03  APPL-P-TOT03            PIC S9(7)    COMP VALUE 0.
           03  APPL-P-TOT04            PIC S9(7)    COMP VALUE 0.
           03  APPL-P-TOT05            PIC S9(7)    COMP VALUE 0.
           03  APPL-P-TOT06            PIC S9(7)    COMP VALUE 0.
           03  APPL-P-TOT07            PIC S9(7)    COMP VALUE 0.
           03  APPL-P-TOT08            PIC S9(3)V99 COMP VALUE 0.
           03  APPL-P-TOT09            PIC S9(7)    COMP VALUE 0.
           03  APPL-P-TOT10            PIC S9(3)V99 COMP VALUE 0.

       01  P6-TOTALS.
           03  REPO-P-TOTOUTS          PIC S9(8)V99 COMP VALUE 0.
           03  REPO-P-TOTPRIN-MTD      PIC S9(8)V99 COMP VALUE 0.
           03  REPO-P-TOTPRIN-YTD      PIC S9(8)V99 COMP VALUE 0.
           03  REPO-P-TOTPRIN-PCT      PIC S9(3)V99 COMP VALUE 0.
           03  REPO-P-TOTVOL-MTD       PIC S9(8)V99 COMP VALUE 0.
           03  REPO-P-TOTVOL-YTD       PIC S9(8)V99 COMP VALUE 0.
           03  REPO-P-TOTVOL-LYR       PIC S9(8)V99 COMP VALUE 0.
           03  REPO-P-TOTVOL-PCT       PIC S9(3)V99 COMP VALUE 0.
           03  REPO-P-TOTACCTS         PIC S9(6)    COMP VALUE 0.
           03  REPO-P-TOTACCTS-MTD     PIC S9(5)    COMP VALUE 0.
           03  REPO-P-TOTACCTS-YTD     PIC S9(6)    COMP VALUE 0.
           03  REPO-P-TOTNWACCTS-MTD   PIC S9(6)    COMP VALUE 0.
           03  REPO-P-TOTNWACCTS-YTD   PIC S9(6)    COMP VALUE 0.
           03  REPO-P-TOTNWACCTS-LYR   PIC S9(6)    COMP VALUE 0.
      /
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH          PIC 9(4).
           03  END-BRANCH          PIC 9(4).
           03  BEG-CLASS           PIC 99.
           03  SOURCECD            PIC X.
           03  SRVCACCT            PIC X.
           03  PL-ISO              PIC X.
           03  REPO-ISO            PIC X.
           03  SUMMARY-ONLY        PIC X.
           03  MONTH-DATE          PIC 9(4).
           03  MONTH-DATEX     REDEFINES MONTH-DATE.
               05  MONTH-MO        PIC 9(2).
               05  MONTH-YR        PIC 9(2).
           03  GROUP-FLAG          PIC X.
           03  ENT-GROUP           PIC X(4).
           03  INCL-REPO-WITH-OUTS PIC X(01).
               88 INCL-REPO-WITH-OUTS-Y  VALUE "Y".
               88 INCL-REPO-WITH-OUTS-VALID   VALUE "Y" "N".
           03  END-CLASS           PIC 99.
           03  END-CLASSX REDEFINES END-CLASS PIC XX.

       01  VERSION-CONTROL     PIC X     VALUE "B".
       01  SV-BEG-BR           PIC 9(4)  VALUE 9999.
       01  SV-END-BR           PIC 9(4)  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

       COPY "LIBGB/PASSWDW.CPY".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99        VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(51) VALUE
           "- - - -  BRANCH ANALYSIS REPORT   - - - -     FOR: ".
           03  H-MMYY           PIC 9999.

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRNAME         PIC X(20) VALUE SPACES.

       01  HEAD4.
           03  SECTION-TITLE    PIC X(13) VALUE SPACES.
           03  FILLER           PIC X(18) VALUE SPACES.
           03  FILLER           PIC X(9)  VALUE "PRINCIPAL".
           03  FILLER           PIC X(62) VALUE SPACES.
           03  FILLER           PIC X(7)  VALUE "ACCOUNT".

       01  HEAD5.
           03  FILLER              PIC X(5)  VALUE " LOAN".
           03  FILLER              PIC X(21) VALUE SPACES.
           03  FILLER              PIC X(18) VALUE
               "---- GAIN/LOSS ---".
           03  FILLER              PIC X(13) VALUE SPACES.
           03  FILLER              PIC X(30) VALUE
               "--------- NEW $VOLUME --------".
           03  FILLER              PIC X(14) VALUE SPACES.
           03  FILLER              PIC X(9)  VALUE "GAIN/LOSS".
           03  FILLER              PIC X(5)  VALUE SPACES.
           03  FILLER              PIC X(16) VALUE
               "- NEW ACCOUNTS -".

       01  HEAD6.
           03  FILLER          PIC X(41) VALUE
           "CLASS OUTSTANDING         MTD         YTD".
           03  FILLER          PIC X(40) VALUE
           "    %           MTD         YTD         ".
           03  FILLER          PIC X(40) VALUE
           "LYR    %    ACCTS   MTD    YTD    MTD   ".
           03  FILLER          PIC X(12) VALUE
               " YTD    LYR".

       01  HEAD7.
           03  FILLER          PIC X(35) VALUE
               "LOAN        - OTHER -  - INTEREST -".
           03  FILLER          PIC X(42) VALUE
               "  - LATE CHG - - PRINCIPAL-  - DEFERMENT -".
           03  FILLER          PIC X(28) VALUE
               " -P&L RECOVR-    - OTHER 2 -".
           03  FILLER          PIC X(27) VALUE
               "   -- TOTALS --   -RENEWED-".

       01  HEAD8.
           03  FILLER          PIC X(20) VALUE
               "LOAN    --INTEREST- ".
           03  FILLER          PIC X(24) VALUE
               "-CRED LIFE- -ACC HEALTH ".
           03  FILLER          PIC X(22) VALUE
               "-PERS PROP- -OTHER 1- ".
           03  FILLER          PIC X(32) VALUE
               "-OTHER 2- -OTHER 3- -SRV CHARGE ".
           03  FILLER          PIC X(22) VALUE
               "-MNT FEE- -DEFERMENT- ".
           03  FILLER          PIC X(8) VALUE
               "-TOTALS-".

       01  HEAD9A.
           03  FILLER          PIC X(47) VALUE
           "CLASS       MTD/YTD       MTD/YTD       MTD/YTD".
           03  FILLER          PIC X(46) VALUE
           "       MTD/YTD       MTD/YTD       MTD/YTD  ".
           03  FILLER          PIC X(38) VALUE
           "   MTD/YTD       MTD/YTD       MTD/YTD".

       01  HEAD9B.
           03  FILLER          PIC X(41) VALUE
           "CLASS     MTD/YTD     MTD/YTD     MTD/YTD".
           03  FILLER          PIC X(33) VALUE
           "     MTD/YTD    MTD/YTD   MTD/YTD".
           03  FILLER          PIC X(32) VALUE
           "   MTD/YTD    MTD/YTD    MTD/YTD".
           03  FILLER          PIC X(22) VALUE
           "    MTD/YTD    MTD/YTD".

       01  HEAD10.
           03  FILLER          PIC X(42) VALUE
               "LOAN        ---TAKEN--         -TURN DOWN-".
           03  FILLER          PIC X(43) VALUE
               "        NOT TAKEN                LOANS MADE".

       01  HEAD11.
           03  FILLER          PIC X(42) VALUE
               "CLASS      MTD      YTD       MTD      YTD".
           03  FILLER          PIC X(49) VALUE
               "       MTD      YTD       MTD----%       YTD----%".
       01  HEAD12.
           03  HEAD-12-TAG     PIC X(15) VALUE SPACES.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
       01  DETAIL1-2           REDEFINES DETAIL-LINE.
           03  D1-2-TAG1.
               05  D1-2-SOURCE       PIC XXXX.
               05  FILLER            PIC X(1).
           03  D1-2-TAG2            REDEFINES D1-2-TAG1.
               05  D1-2-CL           PIC XX.
               05  D1-2-CLASS        PIC 99.
               05  FILLER            PIC X.
           03  D1-2-TAG3            REDEFINES D1-2-TAG1.
               05  FILLER            PIC X(5).
           03  D1-2-DATA.
               05  D1-2-OUTSTANDX.
                   07  D1-2-OUTSTAND     PIC Z(8)9.99-  BLANK ZERO.
               05  D1-2-PRIN-MTDX.
                   07  D1-2-PRIN-MTD     PIC Z(7)9.99-  BLANK ZERO.
               05  D1-2-PRIN-YTDX.
                   07  D1-2-PRIN-YTD     PIC Z(7)9.99-  BLANK ZERO.
               05  D1-2-PRIN-PCTX.
                   07  D1-2-PRIN-PCT     PIC ZZ9.99-    BLANK ZERO.
               05  D1-2-VOL-MTDX.
                   07  D1-2-VOL-MTD      PIC Z(7)9.99-  BLANK ZERO.
               05  D1-2-VOL-YTDX.
                   07  D1-2-VOL-YTD      PIC Z(7)9.99-  BLANK ZERO.
               05  D1-2-VOL-LYRX.
                   07  D1-2-VOL-LYR      PIC Z(7)9.99-  BLANK ZERO.
               05  D1-2-VOL-PCTX.
                   07  D1-2-VOL-PCT      PIC ZZ9.99-    BLANK ZERO.
               05  D1-2-ACCTSX.
                   07  D1-2-ACCTS        PIC Z(5)9-     BLANK ZERO.
               05  D1-2-ACCTS-MTDX.
                   07  D1-2-ACCTS-MTD    PIC Z(4)9-     BLANK ZERO.
               05  D1-2-ACCTS-YTDX.
                   07  D1-2-ACCTS-YTD    PIC Z(5)9-     BLANK ZERO.
               05  D1-2-NWACTS-MTDX.
                   07  D1-2-NWACTS-MTD   PIC Z(5)9-     BLANK ZERO.
               05  D1-2-NWACTS-YTDX.
                   07  D1-2-NWACTS-YTD   PIC Z(5)9-     BLANK ZERO.
               05  D1-2-NWACTS-LYRX.
                   07  D1-2-NWACTS-LYR   PIC Z(5)9-     BLANK ZERO.

       01  DETAIL3-4                  REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X.
           03  D3-4-CL                 PIC XX.
           03  D3-4-CLASS              PIC 99.
           03  D3-4-DATA               PIC X(127).
           03  D3-DATA                REDEFINES D3-4-DATA.
               05  FILLER              PIC X(3).
               05  D3-OTH-MTD          PIC Z(7)9.99-  BLANK ZERO.
               05  FILLER              PIC XX.
               05  D3-INT-MTD          PIC Z(7)9.99-  BLANK ZERO.
               05  FILLER              PIC XX.
               05  D3-LAT-MTD          PIC Z(7)9.99-  BLANK ZERO.
               05  FILLER              PIC XX.
               05  D3-PRI-MTD          PIC Z(7)9.99-  BLANK ZERO.
               05  FILLER              PIC XX.
               05  D3-DEF-MTD          PIC Z(7)9.99-  BLANK ZERO.
               05  FILLER              PIC XX.
               05  D3-PLA-MTD          PIC Z(7)9.99-  BLANK ZERO.
               05  FILLER              PIC XX.
               05  D3-PLI-MTD          PIC Z(7)9.99-  BLANK ZERO.
               05  FILLER              PIC XX.
               05  D3-TOT-MTD          PIC Z(7)9.99-  BLANK ZERO.
               05  FILLER              PIC XX.
               05  D3-RNP-MTD          PIC Z(7)9.99-  BLANK ZERO.
           03  D4-DATA                REDEFINES D3-4-DATA.
               05  FILLER              PIC X(3).
               05  D4-INT-MTD          PIC Z(6)9.99-  BLANK ZERO.
               05  D4-CL-MTD           PIC Z(6)9.99-  BLANK ZERO.
               05  D4-AH-MTD           PIC Z(6)9.99-  BLANK ZERO.
               05  D4-PP-MTD           PIC Z(6)9.99-  BLANK ZERO.
               05  D4-O1-MTD           PIC Z(6)9.99-  BLANK ZERO.
               05  D4-O2-MTD           PIC Z(6)9.99-  BLANK ZERO.
               05  D4-O3-MTD           PIC Z(6)9.99-  BLANK ZERO.
               05  D4-SC-MTD           PIC Z(6)9.99-  BLANK ZERO.
               05  D4-MF-MTD           PIC Z(6)9.99-  BLANK ZERO.
               05  D4-DF-MTD           PIC Z(6)9.99-  BLANK ZERO.
               05  D4-TOT-MTD          PIC Z(6)9.99-  BLANK ZERO.
               05  FILLER              PIC X(2).

       01  DETAIL5                  REDEFINES DETAIL-LINE.
           03  FILLER                PIC X.
           03  D5-CL                 PIC XX.
           03  D5-CLASS              PIC 99.
           03  FILLER                PIC XX.
           03  D5-DATA.
               05  D5-TAKEN-MTD      PIC Z(6)9-  BLANK ZERO.
               05  FILLER            PIC X.
               05  D5-TAKEN-YTD      PIC Z(6)9-  BLANK ZERO.
               05  FILLER            PIC XX.
               05  D5-TDOWN-MTD      PIC Z(6)9-  BLANK ZERO.
               05  FILLER            PIC X.
               05  D5-TDOWN-YTD      PIC Z(6)9-  BLANK ZERO.
               05  FILLER            PIC XX.
               05  D5-NOTAKEN-MTD    PIC Z(6)9-  BLANK ZERO.
               05  FILLER            PIC X.
               05  D5-NOTAKEN-YTD    PIC Z(6)9-  BLANK ZERO.
               05  FILLER            PIC XX.
               05  D5-LOANS-MTD      PIC Z(6)9-  BLANK ZERO.
               05  D5-LOANS-MPCT     PIC ZZ9.99- BLANK ZERO.
               05  D5-LOANS-YTD      PIC Z(6)9-  BLANK ZERO.
               05  D5-LOANS-YPCT     PIC ZZ9.99- BLANK ZERO.



      * STAGGER CORPORATE TOTALS, NOT ENOUGH ROOM !
       01  DETAIL1-6           REDEFINES DETAIL-LINE.
           03  FILLER                PIC X(15).
           03  C1-2-PRIN-MTDX.
               05  C1-2-PRIN-MTD     PIC Z(10)9.99-  BLANK ZERO.
           03  FILLER                PIC X(16).
           03  C1-2-VOL-MTDX.
               05  C1-2-VOL-MTD      PIC Z(10)9.99-  BLANK ZERO.
           03  FILLER                PIC X(9).
           03  C1-2-VOL-LYRX.
               05  C1-2-VOL-LYR      PIC Z(10)9.99-  BLANK ZERO.
           03  C1-2-VOL-PCTX.
               05  C1-2-VOL-PCT      PIC ZZ9.99-    BLANK ZERO.
           03  C1-2-ACCTSX.
               05  C1-2-ACCTS        PIC Z(5)9-     BLANK ZERO.
           03  C1-2-ACCTS-MTDX.
               05  C1-2-ACCTS-MTD    PIC Z(4)9-     BLANK ZERO.
           03  C1-2-ACCTS-YTDX.
               05  C1-2-ACCTS-YTD    PIC Z(5)9-     BLANK ZERO.
           03  C1-2-NWACTS-MTDX.
               05  C1-2-NWACTS-MTD   PIC Z(5)9-     BLANK ZERO.
           03  C1-2-NWACTS-YTDX.
               05  C1-2-NWACTS-YTD   PIC Z(5)9-     BLANK ZERO.
           03  C1-2-NWACTS-LYRX.
               05  C1-2-NWACTS-LYR   PIC Z(5)9-     BLANK ZERO.

      * STAGGER CORPORATE TOTALS, NOT ENOUGH ROOM !
       01  DETAIL1-7           REDEFINES DETAIL-LINE.
           03  FILLER                PIC X(3).
           03  C1-2-OUTSTANDX.
               05  C1-2-OUTSTAND     PIC Z(10)9.99-  BLANK ZERO.
           03  FILLER                PIC X(9).
           03  C1-2-PRIN-YTDX.
               05  C1-2-PRIN-YTD     PIC Z(10)9.99-  BLANK ZERO.
           03  C1-2-PRIN-PCTX.
               05  C1-2-PRIN-PCT     PIC ZZ9.99-    BLANK ZERO.
           03  FILLER                PIC X(9).
           03  C1-2-VOL-YTDX.
               05  C1-2-VOL-YTD      PIC Z(10)9.99-  BLANK ZERO.

       01  RANGE-LINE               REDEFINES DETAIL-LINE.
           03  FILLER                PIC X(30).
           03  RANGE-SEL             PIC X(6).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BYANRP_DEF.CPY".
       COPY "LIBLP/BYANRP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/BYANRP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
      *    PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-BY1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *******************************************************
       MAIN-MODULE SECTION.
      *******************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BYANRP" TO VDU-FORM-NAME.

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

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "REPORT IN PROGRESS................", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BY-PATH-OVERRIDE.

           PERFORM OPEN-BY1-FILE.

           MOVE BEG-BRANCH TO SAVE-BEG-BR.
           PERFORM BEGIN-BY-FILE.
           PERFORM GET-BRANCH.

       NEXT-BY1.
           PERFORM READ-BY1-FILE-NEXT.
           IF IO-BAD
              MOVE "X" TO E-O-FLAG
              GO TO TEST-END-ROUTINE.

      *    STRING BY-DATE-CCYYMM," - ",BY-BRNO," - ",BY-CLASS
      *           DELIMITED BY "  " INTO MESS.
      *    PERFORM SEND-LEGEND.
      *

           IF BY-DATE-CCYYMM NOT = HOLD-BY-DATE-CCYYMM
              GO TO TEST-END-ROUTINE.

           IF BY-BRNO > BR-NO
              GO TO TEST-END-ROUTINE.

           IF END-CLASSX = SPACES
              IF BEG-CLASS NOT = 0
                 IF BY-CLASS NOT = BEG-CLASS
                    GO TO NEXT-BY1.
           IF END-CLASSX NOT = SPACES
              IF BY-CLASS < BEG-CLASS OR BY-CLASS > END-CLASS
                 GO TO NEXT-BY1.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE BY-BRNO TO HOLD-BRANCH
              PERFORM GET-BRANCH.

           IF BY-BRNO NOT = HOLD-BRANCH
              GO TO TEST-END-ROUTINE.

       CLASS-TEST.
           MOVE BY-CLASS TO LC-CODE.
      *    PERFORM READ-LC1-FILE.

           IF (ACCT-TYPE = 1 OR 2) AND ( LC-SERVACCT = "Y" )
              GO TO NEXT-BY1.

           IF ( ACCT-TYPE = 1 ) AND ( LC-SALEFIN = "N" ) AND
              ( LC-SERVACCT = "N" )
              GO TO OK-TO-PROCESS.

           IF ( ACCT-TYPE = 2 ) AND ( LC-SALEFIN = "Y" )
              GO TO OK-TO-PROCESS.

           IF ( ACCT-TYPE = 3 ) AND ( LC-SERVACCT = "Y" )
              GO TO OK-TO-PROCESS.

           GO TO NEXT-BY1.

       OK-TO-PROCESS.
           PERFORM BY1-DETAIL.

           GO TO NEXT-BY1.

       TEST-END-ROUTINE.
           PERFORM PART-TOTALS.

           ADD 1 TO ACCT-TYPE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *   IF MORE PARTS, START AT BEG OF THIS BRANCH & FALL THROUGH AGAIN
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ( ACCT-TYPE NOT > MAX-PARTS-PER-SECTION )
               PERFORM BEGIN-BY-FILE
               GO TO NEXT-BY1.

           PERFORM SECTION-TOTALS.

      *----------------------------------------------------------------
       ZERO-TOTALS.
           PERFORM ZERO-S1-TOTALS.
           PERFORM ZERO-S2-TOTALS.
           PERFORM ZERO-S3-TOTALS.
           PERFORM ZERO-S4-TOTALS.
           PERFORM ZERO-S5-TOTALS.
           PERFORM ZERO-S6-TOTALS.
           PERFORM ZERO-P1-TOTALS.
           PERFORM ZERO-P2-TOTALS.
           PERFORM ZERO-P3-TOTALS.
           PERFORM ZERO-P4-TOTALS.
           PERFORM ZERO-P5-TOTALS.
           PERFORM ZERO-P6-TOTALS.

      *----------------------------------------------------------------
       ZERO-TOTALS-X.
           MOVE FIRST-PART TO ACCT-TYPE.
           ADD 1 TO SECTION-SUB.

           IF ( SECTION-SUB = 2 ) AND ( PL-ISO = "S" )
               ADD 1 TO SECTION-SUB.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *   END OF FILE AND END OF SECTIONS - PERFORM GRAND TOTALS, QUIT
      *   END OF REQUESTED BRANCHES AND END OF SECTIONS -
      *           PERFORM GRAND TOTALS, QUIT
      *   IF MORE SECTIONS, START AT BEG OF THIS BR & FALL THROUGH
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ( SECTION-SUB NOT > MAX-SECTIONS-PER-BR )
               PERFORM BEGIN-BY-FILE
               PERFORM HEAD-PAGE
               GO TO NEXT-BY1.

           IF ( SECTION-SUB > MAX-SECTIONS-PER-BR )
               IF (E-O-F) OR (BY-BRNO > END-BRANCH) OR
                              (BY-DATE-CCYYMM NOT = HOLD-BY-DATE-CCYYMM)
                   GO TO END-OF-GROUP
               ELSE
                   GO TO END-OF-GROUP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *   PROCESS THIS (NEXT) BRANCH
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE FIRST-SECTION TO SECTION-SUB.
           PERFORM GET-BRANCH.
           MOVE 99 TO LN-WK.
           PERFORM HEAD-PAGE.

           GO TO CLASS-TEST.

      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM ZERO-TOTALS.
           MOVE FIRST-SECTION TO SECTION-SUB.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           PERFORM CLOSE-BY1-FILE.

           GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM ZERO-TOTALS.
           PERFORM GRAND-TOTALS.
           PERFORM RANGE-LINES.
           GO TO END-PROGRAM.

      ***************************
       INITIALIZATION SECTION.
      ***************************
      *    PERFORM OPEN-LC1-FILE.

           MOVE BYANRP-QUEUE-POS  TO Q-POS.
           MOVE BYANRP-COPIES-POS TO C-POS.
           MOVE BYANRP-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           MOVE MONTH-MO TO DATE-MMDDYY-MM.
           MOVE MONTH-YR TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM HOLD-BY-DATE-CCYYMM.


           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE MONTH-DATE TO H-MMYY.

           IF PL-ISO = "O"
              MOVE 2 TO FIRST-SECTION
              MOVE 2 TO MAX-SECTIONS-PER-BR.
           IF SRVCACCT = "O"
              MOVE 3 TO FIRST-PART.

           MOVE FIRST-SECTION TO SECTION-SUB.
           MOVE FIRST-PART TO ACCT-TYPE.
           IF SRVCACCT = "S"
               MOVE 2 TO MAX-PARTS-PER-SECTION
           ELSE
               MOVE 3 TO MAX-PARTS-PER-SECTION.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS................" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

      *************************
       EO-EXEC-AUDIT SECTION.
      *************************
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO MONTH-MO.
           MOVE S-YY TO MONTH-YR.
           MOVE MONTH-DATE TO DISPLAY-DATE.

      ****************************
       ENTER-PARAMETERS SECTION.
      ****************************
           IF VDUSTATUS = "1U" GO TO SUMRY.
      *-------------------------------------------------
       SEND-DEFAULT-DATE.
           IF EXT-ROUTE-BUF = "00"
              PERFORM GET-TODAYS-DATE
              GO TO BR-01.
      *-------------------------------------------------
       ENTER-DATE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO BR-01.
           MOVE "EN N040MMYY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DATE.
           MOVE VDUNUM0 TO MONTH-DATE DISPLAY-DATE.
           IF MONTH-MO < 1 OR MONTH-MO > 12
              GO TO ENTER-DATE.
      *----------------------------------------------------------------
       BR-01.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-DATE.

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

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.

           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *-----------------------------------------------------
       CL-1.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLCLS.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF NOT IO-GOOD
                 GO TO CL-1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-1.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-2.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-2.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              GO TO CL-2.

           GO TO SOURCE-CODE.
      *----------------------------------------------------
       ALLCLS.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------
       SOURCE-CODE.
           MOVE "E  A010SOURCD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-2.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SOURCE-CODE.
           MOVE VDUBUF TO SOURCECD.
           IF NOT (SOURCECD = "Y" OR "N") GO TO SOURCE-CODE.
           IF SOURCECD = "Y"
              MOVE "S  A010REPOSUB." TO VDU
              MOVE "N" TO VDUBUF INCL-REPO-WITH-OUTS
              PERFORM SCREENIO
              GO TO SERVIC.
      *----------------------------------------------------
       REPO-SUB.
      *    -------------------------------------------------------------
      *    ONLY ALLOW REPO SUBTOTALS WITH OUTSTANDING WHEN SOURCE CODE
      *    TOTALS ARE -NOT- PRINTED.
      *    -------------------------------------------------------------
           MOVE "E  A010REPOSUB." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SOURCE-CODE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REPO-SUB.
           MOVE VDUBUF TO INCL-REPO-WITH-OUTS.
           IF NOT INCL-REPO-WITH-OUTS-VALID GO TO REPO-SUB.
      *-----------------------------------------------------
       SERVIC.
           MOVE "E  A010SVCACC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF SOURCECD = "Y"
                 GO TO SOURCE-CODE
              ELSE
                 GO TO REPO-SUB.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SERVIC.
           MOVE VDUBUF TO SRVCACCT.
           IF NOT (SRVCACCT = "I" OR "S" OR "O") GO TO SERVIC.
      *--------------------------------------------------------
       P-L.
           MOVE "E  A010PL_INF." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SERVIC.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO P-L.
           MOVE VDUBUF TO PL-ISO.
           IF NOT (PL-ISO = "I" OR "S" OR "O") GO TO P-L.
      *--------------------------------------------------------
       REPO.
           MOVE "E  A010REPO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "P-L" GO TO SERVIC.
           IF VDUSTATUS = "1U" GO TO P-L.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REPO.
           MOVE VDUBUF TO REPO-ISO.
           IF NOT (REPO-ISO = "I" OR "S" OR "O") GO TO REPO.
      *--------------------------------------------------------
       SUMRY.
           MOVE "E  A010SUMARY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REPO.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SUMRY.
           MOVE VDUBUF TO SUMMARY-ONLY.
           IF NOT (SUMMARY-ONLY = "Y" OR "N") GO TO SUMRY.
      *------------------------------------------------------
       EXIT-PARM.
           EXIT.

      *******************************
       DISPLAY-PARAMETERS SECTION.
      *******************************
           MOVE "SNNN040MMYY." TO VDU.
           MOVE MONTH-DATE TO VDUNUM0.
           PERFORM SCREENIO.
      *--------------------------------------------------
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

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
      *--------------------------------------------------
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *--------------------------------------------------
           MOVE "S  A010SOURCD." TO VDU.
           MOVE SOURCECD TO VDUBUF.
           PERFORM SCREENIO.
      *--------------------------------------------------
           MOVE "S  A010REPOSUB." TO VDU.
           MOVE INCL-REPO-WITH-OUTS TO VDUBUF.
           PERFORM SCREENIO.
      *--------------------------------------------------
           MOVE "S  A010SVCACC." TO VDU.
           MOVE SRVCACCT TO VDUBUF.
           PERFORM SCREENIO.
      *--------------------------------------------------
           MOVE "S  A010PL_INF." TO VDU.
           MOVE PL-ISO TO VDUBUF.
           PERFORM SCREENIO.
      *--------------------------------------------------
           MOVE "S  A010REPO." TO VDU.
           MOVE REPO-ISO TO VDUBUF.
           PERFORM SCREENIO.
      *--------------------------------------------------
           MOVE "S  A010SUMARY." TO VDU.
           MOVE SUMMARY-ONLY TO VDUBUF.
           PERFORM SCREENIO.

      ********************************
       BY1-DETAIL SECTION.
      ********************************
           PERFORM COMPUTE-IT.
           PERFORM TOTALS.
           PERFORM PRINT-SECTIONS.
      *----------------------------------------------------------------
      *   JUST CREATE TOTALS RECORD DURING FIRST SECTION'S
      *   CYCLE-NO NEED TO RE-DO WHEN RE-READING FILE
      *   FOR OTHER SECTIONS, AS TOTALS WILL BE INCLUSIVE
      *   FOR ALL SECTIONS (CONTAINED IN REC).
      *----------------------------------------------------------------
           IF CORP-TOTALS
               GO TO BY1-DETAIL-EXIT.
           IF SECTION-SUB = FIRST-SECTION
               PERFORM CREATE-TOTAL-RECS.
       BY1-DETAIL-EXIT.
           EXIT.

      **********************
       COMPUTE-IT SECTION.
      **********************
           PERFORM CLEAR-SECTION-WORKERS.
           EVALUATE SECTION-SUB
              WHEN 1 PERFORM COMPUTE-SEC-1
                     IF INCL-REPO-WITH-OUTS-Y
                        PERFORM COMPUTE-SEC-6
                     END-IF
              WHEN 2 PERFORM COMPUTE-SEC-2
              WHEN 3 PERFORM COMPUTE-SEC-3
              WHEN 4 PERFORM COMPUTE-SEC-4
              WHEN 5 PERFORM COMPUTE-SEC-5
              WHEN 6 PERFORM COMPUTE-SEC-6
           END-EVALUATE.
       COMPUTE-IT-EXIT.
           EXIT.

      **************************
       COMPUTE-SEC-1 SECTION.
      **************************
           MOVE 0 TO SRC-OUTS        SRC-PRIN-MTD    SRC-PRIN-YTD
                     SRC-PRIN-PCT    SRC-VOL-MTD     SRC-VOL-YTD
                     SRC-VOL-LYR     SRC-VOL-PCT     SRC-ACCTS
                     SRC-ACCTS-MTD   SRC-ACCTS-YTD   SRC-NWACCTS-MTD
                     SRC-NWACCTS-YTD SRC-NWACCTS-LYR.
           PERFORM COMPUTE-SD VARYING K FROM 1 BY 1 UNTIL K > 11.
           MOVE SRC-OUTS        TO OUTS-OUTS.
           MOVE SRC-PRIN-MTD    TO OUTS-PRIN-MTD.
           MOVE SRC-PRIN-YTD    TO OUTS-PRIN-YTD.
           MOVE SRC-PRIN-PCT    TO OUTS-PRIN-PCT.
           MOVE SRC-VOL-MTD     TO OUTS-VOL-MTD.
           MOVE SRC-VOL-YTD     TO OUTS-VOL-YTD.
           MOVE SRC-VOL-LYR     TO OUTS-VOL-LYR.
           MOVE SRC-VOL-PCT     TO OUTS-VOL-PCT.
           MOVE SRC-ACCTS       TO OUTS-ACCTS.
           MOVE SRC-ACCTS-MTD   TO OUTS-ACCTS-MTD.
           MOVE SRC-ACCTS-YTD   TO OUTS-ACCTS-YTD.
           MOVE SRC-NWACCTS-MTD TO OUTS-NWACCTS-MTD.
           MOVE SRC-NWACCTS-YTD TO OUTS-NWACCTS-YTD.
           MOVE SRC-NWACCTS-LYR TO OUTS-NWACCTS-LYR.

      *******************
       COMPUTE-SD SECTION.
      *******************

           MOVE BY-AMT(1, 1, K) TO OUTS-OUTS.
           COMPUTE OUTS-PRIN-MTD = BY-AMT(1, 1, K) - BY-AMT(2, 1, K).
           COMPUTE OUTS-PRIN-YTD = BY-AMT(1, 1, K) - BY-AMT(3, 1, K).
           IF BY-AMT(3, 1, K) = 0
              MOVE 0 TO OUTS-PRIN-PCT
           ELSE
              COMPUTE OUTS-PRIN-PCT =
                               OUTS-PRIN-YTD / BY-AMT(3, 1, K) * 100.

           MOVE BY-AMT(1, 2, K) TO OUTS-VOL-MTD.
           MOVE BY-AMT(2, 2, K) TO OUTS-VOL-YTD.
           MOVE BY-AMT(3, 2, K) TO OUTS-VOL-LYR.
           IF OUTS-VOL-LYR = 0
               MOVE 0 TO OUTS-VOL-PCT
           ELSE
               COMPUTE OUTS-VOL-PCT = OUTS-VOL-YTD / OUTS-VOL-LYR * 100.
           MOVE BY-NO(1, 1, K) TO OUTS-ACCTS.
           COMPUTE OUTS-ACCTS-MTD = BY-NO(1, 1, K) - BY-NO(2, 1, K).
           COMPUTE OUTS-ACCTS-YTD = BY-NO(1, 1, K) - BY-NO(3, 1, K).
           MOVE BY-NO(1, 2, K) TO OUTS-NWACCTS-MTD.
           MOVE BY-NO(2, 2, K) TO OUTS-NWACCTS-YTD.
           MOVE BY-NO(3, 2, K) TO OUTS-NWACCTS-LYR.
           IF SOURCECD = "Y"
               PERFORM PRINT-SECTIONS.
           PERFORM SOURCE1-TOTALS.

      *************************
       COMPUTE-SEC-2 SECTION.
      *************************
           PERFORM GET-TOT-PL.

           COMPUTE PL-OUTS =   TOT-AMT-PLA(1 1)
                             + TOT-AMT-PLI(1 1).

           COMPUTE PL-PRIN-MTD =   TOT-AMT-PLA(1 1)
                                 - TOT-AMT-PLA(2 1)
                                 + TOT-AMT-PLI(1 2).

           COMPUTE PL-PRIN-YTD =   TOT-AMT-PLA(1 1)
                                 - TOT-AMT-PLA(3 1)
                                 + TOT-AMT-PLI(2 2).

           IF TOT-AMT-PLA(3 1) + TOT-AMT-PLI(3 1) = 0
               MOVE 0 TO PL-PRIN-PCT
           ELSE
               COMPUTE PL-PRIN-PCT = PL-PRIN-YTD / (TOT-AMT-PLA(3 1) +
                                TOT-AMT-PLI(3 1)).

           MOVE TOT-AMT-PLA(1 2) TO PL-VOL-MTD.
           MOVE TOT-AMT-PLA(2 2) TO PL-VOL-YTD.
           MOVE TOT-AMT-PLA(3 2) TO PL-VOL-LYR.

           IF PL-VOL-LYR = 0
               MOVE 0 TO PL-VOL-PCT
           ELSE
               COMPUTE PL-VOL-PCT = PL-VOL-YTD / PL-VOL-LYR.

           COMPUTE PL-ACCTS = TOT-NO-PLA(1 1) + TOT-NO-PLI(1 1).

           COMPUTE PL-ACCTS-MTD = TOT-NO-PLA(1 1) - TOT-NO-PLA(2 1)
                                                  + TOT-NO-PLI(1 2).

           COMPUTE PL-ACCTS-YTD = TOT-NO-PLA(1 1) - TOT-NO-PLA(3 1)
                                                  + TOT-NO-PLI(2 2).
           MOVE TOT-NO-PLA(1 2) TO PL-NWACCTS-MTD.
           MOVE TOT-NO-PLA(2 2) TO PL-NWACCTS-YTD.
           MOVE TOT-NO-PLA(3 2) TO PL-NWACCTS-LYR.

      **************************
       COMPUTE-SEC-3 SECTION.
      **************************
           MOVE BY-OTHCOL(1)   TO COL-OTH-MTD.
           MOVE BY-OTHCOL(2)   TO COL-OTH-YTD.
           MOVE BY-INTCOL(1)   TO COL-INT-MTD.
           MOVE BY-INTCOL(2)   TO COL-INT-YTD.
           MOVE BY-LATECOL(1)  TO COL-LATE-MTD.
           MOVE BY-LATECOL(2)  TO COL-LATE-YTD.

           MOVE BY-PRINCOL(1)  TO COL-PRIN-MTD.
           SUBTRACT BY-OT2COL(1) FROM COL-PRIN-MTD.

           MOVE BY-PRINCOL(2)  TO COL-PRIN-YTD.
           SUBTRACT BY-OT2COL(2) FROM COL-PRIN-YTD.

           MOVE BY-DEFCOL(1)   TO COL-DEF-MTD.
           MOVE BY-DEFCOL(2)   TO COL-DEF-YTD.

      * P&L RECOVERIES:
           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              COMPUTE COL-PLA-MTD = COL-PLA-MTD
                  + (BY-PL-PRINCOL(1 SUB1)) - BY-PL-O2COL(1 SUB1)
                  +  BY-PL-INTCOL(1 SUB1)   + BY-PL-LATECOL(1 SUB1)
                  +  BY-PL-O2COL(1 SUB1)
              COMPUTE COL-PLA-YTD = COL-PLA-YTD
                  + (BY-PL-PRINCOL(2 SUB1)) - BY-PL-O2COL(2 SUB1)
                  +  BY-PL-INTCOL(2 SUB1)   + BY-PL-LATECOL(2 SUB1)
                  +  BY-PL-O2COL(2 SUB1)
           END-PERFORM.

           MOVE BY-OT2COL(1)   TO COL-PLI-MTD.
           MOVE BY-OT2COL(2)   TO COL-PLI-YTD.

      * TOTAL COLLECTED:
           COMPUTE COL-TOT-MTD =
                BY-OTHCOL(1)  + BY-OT2COL(1)   + BY-INTCOL(1)
              + BY-LATECOL(1) + (BY-PRINCOL(1) - BY-OT2COL(1))
              + BY-DEFCOL(1)  + COL-PLA-MTD.

           COMPUTE COL-TOT-YTD =
                BY-OTHCOL(2)  + BY-OT2COL(2)   + BY-INTCOL(2)
              + BY-LATECOL(2) + (BY-PRINCOL(2) - BY-OT2COL(2))
              + BY-DEFCOL(2)  + COL-PLA-YTD.

           MOVE BY-RNPRIN(1)   TO COL-RNP-MTD.
           MOVE BY-RNPRIN(2)   TO COL-RNP-YTD.

      **************************
       COMPUTE-SEC-4 SECTION.
      **************************
              MOVE BY-INTREB(1) TO REB-INT-1.
              MOVE BY-INTREB(2) TO REB-INT-2.
              COMPUTE REB-CL-1 =
                   BY-CLREB1(1) + BY-CLREB2(1)
                 + BY-CLREB3(1) + BY-CLREB4(1).
              COMPUTE REB-CL-2 =
                   BY-CLREB1(2) + BY-CLREB2(2)
                 + BY-CLREB3(2) + BY-CLREB4(2).
              MOVE BY-AHREB(1) TO REB-AH-1.
              MOVE BY-AHREB(2) TO REB-AH-2.
              MOVE BY-PPREB(1) TO REB-PP-1.
              MOVE BY-PPREB(2) TO REB-PP-2.
              MOVE BY-O1REB(1) TO REB-O1-1.
              MOVE BY-O1REB(2) TO REB-O1-2.
              MOVE BY-O2REB(1) TO REB-O2-1.
              MOVE BY-O2REB(2) TO REB-O2-2.
              MOVE BY-O3REB(1) TO REB-O3-1.
              MOVE BY-O3REB(2) TO REB-O3-2.
              MOVE BY-SCREB(1) TO REB-SC-1.
              MOVE BY-SCREB(2) TO REB-SC-2.
              MOVE BY-MFREB(1) TO REB-MF-1.
              MOVE BY-MFREB(2) TO REB-MF-2.
              MOVE BY-DFREB(1) TO REB-DF-1.
              MOVE BY-DFREB(2) TO REB-DF-2.
              COMPUTE REB-TOT-1 =
                   BY-INTREB(1) + BY-CLREB1(1) + BY-CLREB2(1)
                 + BY-CLREB3(1) + BY-CLREB4(1) + BY-AHREB(1)
                 + BY-PPREB(1)  + BY-O1REB(1)  + BY-O2REB(1)
                 + BY-O3REB(1)  + BY-SCREB(1)  + BY-MFREB(1)
                 + BY-DFREB(1).
              COMPUTE REB-TOT-2 =
                   BY-INTREB(2) + BY-CLREB1(2) + BY-CLREB2(2)
                 + BY-CLREB3(2) + BY-CLREB4(2) + BY-AHREB(2)
                 + BY-PPREB(2)  + BY-O1REB(2)  + BY-O2REB(2)
                 + BY-O3REB(2)  + BY-SCREB(2)  + BY-MFREB(2)
                 + BY-DFREB(2).

      **************************
       COMPUTE-SEC-5 SECTION.
      **************************
      *--->MOVE BY-APPS(1) TO APPL-01.
      *--->MOVE BY-APPS(2) TO APPL-02.
      *--->MOVE BY-TDAPP(1) TO APPL-03.
      *--->MOVE BY-TDAPP(2) TO APPL-04.
      *--->MOVE BY-APPROVED-APP(1) TO APPL-05.
      *--->MOVE BY-APPROVED-APP(2) TO APPL-06.
      *--->COMPUTE APPL-07 = BY-APPS(1) - BY-TDAPP(1) -
      *--->                   BY-APPROVED-APP(1).
      *--->IF APPL-01 = 0
      *--->    MOVE 0 TO APPL-08
      *--->ELSE
      *--->    COMPUTE APPL-08 = APPL-07 / APPL-01.
      *--->COMPUTE APPL-09 = BY-APPS(2) - BY-TDAPP(2) -
      *--->                   BY-APPROVED-APP(2).
      *--->IF APPL-02 = 0
      *--->    MOVE 0 TO APPL-10
      *--->ELSE
      *--->    COMPUTE APPL-10 = APPL-09 / APPL-02.

      **************************
       COMPUTE-SEC-6 SECTION.
      **************************
           MOVE 0 TO REPO-TOTOUTS        REPO-TOTPRIN-MTD
                     REPO-TOTPRIN-YTD    REPO-TOTPRIN-PCT
                     REPO-TOTVOL-MTD     REPO-TOTVOL-YTD
                     REPO-TOTVOL-LYR     REPO-TOTVOL-PCT
                     REPO-TOTACCTS       REPO-TOTACCTS-MTD
                     REPO-TOTACCTS-YTD   REPO-TOTNWACCTS-MTD
                     REPO-TOTNWACCTS-YTD REPO-TOTNWACCTS-LYR.

           PERFORM COMPUTE-REPO VARYING K FROM 1 BY 1
               UNTIL K > 3.

           MOVE REPO-TOTOUTS        TO REPO-OUTS.
           MOVE REPO-TOTPRIN-MTD    TO REPO-PRIN-MTD.
           MOVE REPO-TOTPRIN-YTD    TO REPO-PRIN-YTD.
           MOVE REPO-TOTPRIN-PCT    TO REPO-PRIN-PCT.
           MOVE REPO-TOTVOL-MTD     TO REPO-VOL-MTD.
           MOVE REPO-TOTVOL-YTD     TO REPO-VOL-YTD.
           MOVE REPO-TOTVOL-LYR     TO REPO-VOL-LYR.
           MOVE REPO-TOTVOL-PCT     TO REPO-VOL-PCT.

           MOVE REPO-TOTACCTS       TO REPO-ACCTS.
           MOVE REPO-TOTACCTS-MTD   TO REPO-ACCTS-MTD.
           MOVE REPO-TOTACCTS-YTD   TO REPO-ACCTS-YTD.
           MOVE REPO-TOTNWACCTS-MTD TO REPO-NWACCTS-MTD.
           MOVE REPO-TOTNWACCTS-YTD TO REPO-NWACCTS-YTD.
           MOVE REPO-TOTNWACCTS-LYR TO REPO-NWACCTS-LYR.

      ************************
       COMPUTE-REPO SECTION.
      ************************
           MOVE 0 TO REPO-OUTS        REPO-PRIN-MTD
                     REPO-PRIN-YTD    REPO-PRIN-PCT
                     REPO-VOL-MTD     REPO-VOL-YTD
                     REPO-VOL-LYR     REPO-VOL-PCT
                     REPO-ACCTS       REPO-ACCTS-MTD
                     REPO-ACCTS-YTD   REPO-NWACCTS-MTD
                     REPO-NWACCTS-YTD REPO-NWACCTS-LYR.

      *    ------------------------------------------------------
      *    DO NOT INCLUDE GAIN/LOSS REDEEMED (SUB 2) WITH TOTALS
      *    THIS 'IF' STATEMENT WILL ALSO STOP GRAND TOTALS COMING
      *    FROM THE WORK FILE.
      *    ------------------------------------------------------
           IF K NOT = 2
              MOVE BY-AMT-REPO(1, 1, K) TO REPO-OUTS
              COMPUTE REPO-PRIN-MTD = BY-AMT-REPO(1, 1, K)
                                    - BY-AMT-REPO(2, 1, K)
              COMPUTE REPO-PRIN-YTD = BY-AMT-REPO(1, 1, K)
                                    - BY-AMT-REPO(3, 1, K)
              IF BY-AMT-REPO(3, 1, K) = 0
                 MOVE 0 TO REPO-PRIN-PCT
              ELSE
                 COMPUTE REPO-PRIN-PCT =
                           REPO-PRIN-YTD / BY-AMT-REPO(3, 1, K) * 100
              END-IF
              MOVE BY-NO-REPO(1, 1, K) TO REPO-ACCTS
              COMPUTE REPO-ACCTS-MTD = BY-NO-REPO(1, 1, K)
                                     - BY-NO-REPO(2, 1, K)
              COMPUTE REPO-ACCTS-YTD = BY-NO-REPO(1, 1, K)
                                     - BY-NO-REPO(3, 1, K).

           MOVE BY-AMT-REPO(1, 2, K) TO REPO-VOL-MTD.
           MOVE BY-AMT-REPO(2, 2, K) TO REPO-VOL-YTD.
           MOVE BY-AMT-REPO(3, 2, K) TO REPO-VOL-LYR.
           IF REPO-VOL-LYR = 0
               MOVE 0 TO REPO-VOL-PCT
           ELSE
               COMPUTE REPO-VOL-PCT = REPO-VOL-YTD / REPO-VOL-LYR * 100.

           MOVE BY-NO-REPO(1, 2, K) TO REPO-NWACCTS-MTD.
           MOVE BY-NO-REPO(2, 2, K) TO REPO-NWACCTS-YTD.
           MOVE BY-NO-REPO(3, 2, K) TO REPO-NWACCTS-LYR.
           IF SOURCECD = "Y"
               PERFORM PRINT-SECTIONS.
           PERFORM REPO-TOTALS.

      *********************************************************
      *
      *    T O T A L S   S E C T I O N
      *
      *   MOVE TOTALS TO DETAIL LINE TO PREPARE FOR PRINTING.
      *
      *  SECTION-SUB:  1 : LOANS OUT
      *                2 : P & L
      *                3 : COLLECTIONS
      *                4 : REBATES
      *                5 : APPLICATIONS
      *                6 : REPOS
      *                7 : GRAND TOTALS
      *********************************************************
       TOTALS SECTION.
      *********************************************************
           EVALUATE SECTION-SUB
               WHEN 1 PERFORM TOTAL-1
                     IF INCL-REPO-WITH-OUTS-Y
                        PERFORM TOTAL-6
                     END-IF
               WHEN 2 PERFORM TOTAL-2
               WHEN 3 PERFORM TOTAL-3
               WHEN 4 PERFORM TOTAL-4
               WHEN 5 PERFORM TOTAL-5
               WHEN 6 PERFORM TOTAL-6
           END-EVALUATE.
       TOTALS-EXIT.
           EXIT.

      ********************
       TOTAL-1 SECTION.
      ********************
           ADD OUTS-OUTS        TO OUTS-P-TOTOUTS.
           ADD OUTS-PRIN-MTD    TO OUTS-P-TOTPRIN-MTD.
           ADD OUTS-PRIN-YTD    TO OUTS-P-TOTPRIN-YTD.
           ADD OUTS-PRIN-PCT    TO OUTS-P-TOTPRIN-PCT.
           ADD OUTS-VOL-MTD     TO OUTS-P-TOTVOL-MTD.
           ADD OUTS-VOL-YTD     TO OUTS-P-TOTVOL-YTD.
           ADD OUTS-VOL-LYR     TO OUTS-P-TOTVOL-LYR.
           ADD OUTS-VOL-PCT     TO OUTS-P-TOTVOL-PCT.
           ADD OUTS-ACCTS       TO OUTS-P-TOTACCTS.
           ADD OUTS-ACCTS-MTD   TO OUTS-P-TOTACCTS-MTD.
           ADD OUTS-ACCTS-YTD   TO OUTS-P-TOTACCTS-YTD.
           ADD OUTS-NWACCTS-MTD TO OUTS-P-TOTNWACCTS-MTD.
           ADD OUTS-NWACCTS-YTD TO OUTS-P-TOTNWACCTS-YTD.
           ADD OUTS-NWACCTS-LYR TO OUTS-P-TOTNWACCTS-LYR.
       TOTAL-1-EXIT.
           EXIT.

      *******************
       TOTAL-2 SECTION.
      *******************
           ADD PL-OUTS        TO PL-P-TOTOUTS.
           ADD PL-PRIN-MTD    TO PL-P-TOTPRIN-MTD.
           ADD PL-PRIN-YTD    TO PL-P-TOTPRIN-YTD.
           ADD PL-PRIN-PCT    TO PL-P-TOTPRIN-PCT.
           ADD PL-VOL-MTD     TO PL-P-TOTVOL-MTD.
           ADD PL-VOL-YTD     TO PL-P-TOTVOL-YTD.
           ADD PL-VOL-LYR     TO PL-P-TOTVOL-LYR.
           ADD PL-VOL-PCT     TO PL-P-TOTVOL-PCT.
           ADD PL-ACCTS       TO PL-P-TOTACCTS.
           ADD PL-ACCTS-MTD   TO PL-P-TOTACCTS-MTD.
           ADD PL-ACCTS-YTD   TO PL-P-TOTACCTS-YTD.
           ADD PL-NWACCTS-MTD TO PL-P-TOTNWACCTS-MTD.
           ADD PL-NWACCTS-YTD TO PL-P-TOTNWACCTS-YTD.
           ADD PL-NWACCTS-LYR TO PL-P-TOTNWACCTS-LYR.
       TOTAL-2-EXIT.
           EXIT.

      ***********************
       TOTAL-3 SECTION.
      ***********************
           ADD COL-OTH-MTD  TO COL-P-TOTOTH-MTD.
           ADD COL-OTH-YTD  TO COL-P-TOTOTH-YTD.
           ADD COL-INT-MTD  TO COL-P-TOTINT-MTD.
           ADD COL-INT-YTD  TO COL-P-TOTINT-YTD.
           ADD COL-LATE-MTD TO COL-P-TOTLATE-MTD.
           ADD COL-LATE-YTD TO COL-P-TOTLATE-YTD.
           ADD COL-PRIN-MTD TO COL-P-TOTPRIN-MTD.
           ADD COL-PRIN-YTD TO COL-P-TOTPRIN-YTD.
           ADD COL-DEF-MTD  TO COL-P-TOTDEF-MTD.
           ADD COL-DEF-YTD  TO COL-P-TOTDEF-YTD.
           ADD COL-PLA-MTD  TO COL-P-TOTPLA-MTD.
           ADD COL-PLA-YTD  TO COL-P-TOTPLA-YTD.
           ADD COL-PLI-MTD  TO COL-P-TOTPLI-MTD.
           ADD COL-PLI-YTD  TO COL-P-TOTPLI-YTD.
           ADD COL-RNP-MTD  TO COL-P-TOTRNP-MTD.
           ADD COL-RNP-YTD  TO COL-P-TOTRNP-YTD.
           ADD COL-TOT-MTD  TO COL-P-TOTTOT-MTD.
           ADD COL-TOT-YTD  TO COL-P-TOTTOT-YTD.
       TOTAL-3-EXIT.
           EXIT.

      ********************
       TOTAL-4 SECTION.
      ********************
           ADD REB-INT-1 TO REB-P-TOTINT-1.
           ADD REB-INT-2 TO REB-P-TOTINT-2.
           ADD REB-CL-1  TO REB-P-TOTCL-1.
           ADD REB-CL-2  TO REB-P-TOTCL-2.
           ADD REB-AH-1  TO REB-P-TOTAH-1.
           ADD REB-AH-2  TO REB-P-TOTAH-2.
           ADD REB-PP-1  TO REB-P-TOTPP-1.
           ADD REB-PP-2  TO REB-P-TOTPP-2.
           ADD REB-O1-1  TO REB-P-TOTO1-1.
           ADD REB-O1-2  TO REB-P-TOTO1-2.
           ADD REB-O2-1  TO REB-P-TOTO2-1.
           ADD REB-O2-2  TO REB-P-TOTO2-2.
           ADD REB-O3-1  TO REB-P-TOTO3-1.
           ADD REB-O3-2  TO REB-P-TOTO3-2.
           ADD REB-SC-1  TO REB-P-TOTSC-1.
           ADD REB-SC-2  TO REB-P-TOTSC-2.
           ADD REB-MF-1  TO REB-P-TOTMF-1.
           ADD REB-MF-2  TO REB-P-TOTMF-2.
           ADD REB-DF-1  TO REB-P-TOTDF-1.
           ADD REB-DF-2  TO REB-P-TOTDF-2.
           ADD REB-TOT-1 TO REB-P-TOTTOT-1.
           ADD REB-TOT-2 TO REB-P-TOTTOT-2.
       TOTAL-4-EXIT.
           EXIT.

      *********************
       TOTAL-5 SECTION.
      *********************
           ADD APPL-01 TO APPL-P-TOT01.
           ADD APPL-02 TO APPL-P-TOT02.
           ADD APPL-03 TO APPL-P-TOT03.
           ADD APPL-04 TO APPL-P-TOT04.
           ADD APPL-05 TO APPL-P-TOT05.
           ADD APPL-06 TO APPL-P-TOT06.
           ADD APPL-07 TO APPL-P-TOT07.
           ADD APPL-08 TO APPL-P-TOT08.
           ADD APPL-09 TO APPL-P-TOT09.
           ADD APPL-10 TO APPL-P-TOT10.

      *********************
       TOTAL-6 SECTION.
      *********************
           ADD REPO-OUTS         TO REPO-P-TOTOUTS.
           ADD REPO-PRIN-MTD     TO REPO-P-TOTPRIN-MTD.
           ADD REPO-PRIN-YTD     TO REPO-P-TOTPRIN-YTD.
           ADD REPO-PRIN-PCT     TO REPO-P-TOTPRIN-PCT.
           ADD REPO-VOL-MTD      TO REPO-P-TOTVOL-MTD.
           ADD REPO-VOL-YTD      TO REPO-P-TOTVOL-YTD.
           ADD REPO-VOL-LYR      TO REPO-P-TOTVOL-LYR.
           ADD REPO-VOL-PCT      TO REPO-P-TOTVOL-PCT.

           ADD REPO-ACCTS        TO REPO-P-TOTACCTS.
           ADD REPO-ACCTS-MTD    TO REPO-P-TOTACCTS-MTD.
           ADD REPO-ACCTS-YTD    TO REPO-P-TOTACCTS-YTD.
           ADD REPO-NWACCTS-MTD  TO REPO-P-TOTNWACCTS-MTD.
           ADD REPO-NWACCTS-YTD  TO REPO-P-TOTNWACCTS-YTD.
           ADD REPO-NWACCTS-LYR  TO REPO-P-TOTNWACCTS-LYR.
       TOTAL-6-EXIT.
           EXIT.

      *********************************
       SOURCE1-TOTALS SECTION.
      *********************************
           ADD OUTS-OUTS        TO SRC-OUTS.
           ADD OUTS-PRIN-MTD    TO SRC-PRIN-MTD.
           ADD OUTS-PRIN-YTD    TO SRC-PRIN-YTD.
           ADD OUTS-PRIN-PCT    TO SRC-PRIN-PCT.
           ADD OUTS-VOL-MTD     TO SRC-VOL-MTD.
           ADD OUTS-VOL-YTD     TO SRC-VOL-YTD.
           ADD OUTS-VOL-LYR     TO SRC-VOL-LYR.
           ADD OUTS-VOL-PCT     TO SRC-VOL-PCT.
           ADD OUTS-ACCTS       TO SRC-ACCTS.
           ADD OUTS-ACCTS-MTD   TO SRC-ACCTS-MTD.
           ADD OUTS-ACCTS-YTD   TO SRC-ACCTS-YTD.
           ADD OUTS-NWACCTS-MTD TO SRC-NWACCTS-MTD.
           ADD OUTS-NWACCTS-YTD TO SRC-NWACCTS-YTD.
           ADD OUTS-NWACCTS-LYR TO SRC-NWACCTS-LYR.
       SOURCE1-TOTALS-EXIT.
           EXIT.

      *************************
       REPO-TOTALS SECTION.
      *************************
           ADD REPO-OUTS         TO REPO-TOTOUTS.
           ADD REPO-PRIN-MTD     TO REPO-TOTPRIN-MTD.
           ADD REPO-PRIN-YTD     TO REPO-TOTPRIN-YTD.
           ADD REPO-PRIN-PCT     TO REPO-TOTPRIN-PCT.
           ADD REPO-VOL-MTD      TO REPO-TOTVOL-MTD.
           ADD REPO-VOL-YTD      TO REPO-TOTVOL-YTD.
           ADD REPO-VOL-LYR      TO REPO-TOTVOL-LYR.
           ADD REPO-VOL-PCT      TO REPO-TOTVOL-PCT.

           ADD REPO-ACCTS        TO REPO-TOTACCTS.
           ADD REPO-ACCTS-MTD    TO REPO-TOTACCTS-MTD.
           ADD REPO-ACCTS-YTD    TO REPO-TOTACCTS-YTD.
           ADD REPO-NWACCTS-MTD  TO REPO-TOTNWACCTS-MTD.
           ADD REPO-NWACCTS-YTD  TO REPO-TOTNWACCTS-YTD.
           ADD REPO-NWACCTS-LYR  TO REPO-TOTNWACCTS-LYR.
       REPO-TOTALS-EXIT.
           EXIT.

      *****************************************************************
      *
      *                  P R I N T   S E C T I O N
      *
      *  SECTION-SUB:  1 : LOANS OUT
      *                2 : P & L
      *                3 : COLLECTIONS
      *                4 : REBATES
      *                5 : APPLICATIONS
      *                6 : GRAND TOTALS
      *****************************************************************
       PRINT-SECTIONS SECTION.
           EVALUATE SECTION-SUB
                WHEN 1 PERFORM PRINT-SEC1
                WHEN 2 PERFORM PRINT-SEC2
                WHEN 3 PERFORM PRINT-SEC3
                WHEN 4 PERFORM PRINT-SEC4
                WHEN 5 PERFORM PRINT-SEC5
                WHEN 6 PERFORM PRINT-SEC6
           END-EVALUATE.
       PRINT-SECTIONS-EXIT.
           EXIT.

      ***********************
       PRINT-SEC1 SECTION.
      ***********************
           MOVE SPACES           TO DETAIL-LINE.

           MOVE OUTS-OUTS        TO D1-2-OUTSTAND.
           MOVE OUTS-PRIN-MTD    TO D1-2-PRIN-MTD.
           MOVE OUTS-PRIN-YTD    TO D1-2-PRIN-YTD.
           MOVE OUTS-PRIN-PCT    TO D1-2-PRIN-PCT.
           MOVE OUTS-VOL-MTD     TO D1-2-VOL-MTD.
           MOVE OUTS-VOL-YTD     TO D1-2-VOL-YTD.
           MOVE OUTS-VOL-LYR     TO D1-2-VOL-LYR.
           IF OUTS-VOL-LYR = 0
              MOVE 0 TO OUTS-VOL-PCT
           ELSE
              COMPUTE OUTS-VOL-PCT ROUNDED =
                      OUTS-VOL-YTD / OUTS-VOL-LYR * 100.
           MOVE OUTS-VOL-PCT     TO D1-2-VOL-PCT.
           MOVE OUTS-ACCTS       TO D1-2-ACCTS.
           MOVE OUTS-ACCTS-MTD   TO D1-2-ACCTS-MTD.
           MOVE OUTS-ACCTS-YTD   TO D1-2-ACCTS-YTD.
           MOVE OUTS-NWACCTS-MTD TO D1-2-NWACTS-MTD.
           MOVE OUTS-NWACCTS-YTD TO D1-2-NWACTS-YTD.
           MOVE OUTS-NWACCTS-LYR TO D1-2-NWACTS-LYR.

           IF DETAIL-LINE = SPACES
              GO TO PRINT-SEC1-EXIT.

           IF SECTION-SUB = 1 AND
                    (SOURCECD = "Y" AND K < 12)
               MOVE SOURCE-ELE(K) TO D1-2-SOURCE
           ELSE
               MOVE "CL" TO D1-2-CL
               MOVE BY-CLASS TO D1-2-CLASS
               IF INCL-REPO-WITH-OUTS-Y
                  PERFORM SETUP-STAGGERED-TOTALS
                  PERFORM WRITE-DETAIL-LINE
                  PERFORM SETUP-SEC6-SUB-CLASS
                  PERFORM PRINT-SEC6-SUB
               ELSE
                  PERFORM SETUP-STAGGERED-TOTALS.

           PERFORM WRITE-DETAIL-LINE.

      *    -------------------------------------------------------------
      *    SPACE ONE LINE AFTER SOURCE DETAIL TOTALS(SEC1 ONLY)
      *    -------------------------------------------------------------
           IF SECTION-SUB = 1 AND
                  (SOURCECD = "Y" AND K > 11)
               PERFORM WRITE-DETAIL-LINE.
       PRINT-SEC1-EXIT.
           EXIT.

      ***********************************
       SETUP-STAGGERED-TOTALS SECTION.
      ***********************************
      * ONLY STAGGER TOTALS ON CORP TOTALS
           IF NOT CORP-TOTALS
              GO TO SETUP-STAGGERED-TOTALS-EXIT.

           MOVE SPACES           TO DETAIL-LINE.

           MOVE OUTS-PRIN-MTD    TO C1-2-PRIN-MTD.
           MOVE OUTS-VOL-MTD     TO C1-2-VOL-MTD.
           MOVE OUTS-VOL-LYR     TO C1-2-VOL-LYR.
           IF OUTS-VOL-LYR = 0
              MOVE 0 TO OUTS-VOL-PCT
           ELSE
              COMPUTE OUTS-VOL-PCT ROUNDED =
                      OUTS-VOL-YTD / OUTS-VOL-LYR * 100.
           MOVE OUTS-VOL-PCT     TO C1-2-VOL-PCT.
           IF DETAIL-LINE NOT = SPACES
              MOVE "CL" TO D1-2-CL
              MOVE BY-CLASS TO D1-2-CLASS
              PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES           TO DETAIL-LINE.
           MOVE OUTS-OUTS        TO C1-2-OUTSTAND.
           MOVE OUTS-PRIN-YTD    TO C1-2-PRIN-YTD.
           MOVE OUTS-PRIN-PCT    TO C1-2-PRIN-PCT.

           MOVE OUTS-VOL-YTD     TO C1-2-VOL-YTD.

           MOVE OUTS-ACCTS       TO C1-2-ACCTS.
           MOVE OUTS-ACCTS-MTD   TO C1-2-ACCTS-MTD.
           MOVE OUTS-ACCTS-YTD   TO C1-2-ACCTS-YTD.
           MOVE OUTS-NWACCTS-MTD TO C1-2-NWACTS-MTD.
           MOVE OUTS-NWACCTS-YTD TO C1-2-NWACTS-YTD.
           MOVE OUTS-NWACCTS-LYR TO C1-2-NWACTS-LYR.

       SETUP-STAGGERED-TOTALS-EXIT.
           EXIT.

      ***********************
       PRINT-SEC2 SECTION.
      ***********************
           MOVE SPACES         TO DETAIL-LINE.
           MOVE PL-OUTS        TO D1-2-OUTSTAND.
           MOVE PL-PRIN-MTD    TO D1-2-PRIN-MTD.
           MOVE PL-PRIN-YTD    TO D1-2-PRIN-YTD.
           MOVE PL-PRIN-PCT    TO D1-2-PRIN-PCT.
           MOVE PL-VOL-MTD     TO D1-2-VOL-MTD.
           MOVE PL-VOL-YTD     TO D1-2-VOL-YTD.
           MOVE PL-VOL-LYR     TO D1-2-VOL-LYR.
           IF PL-VOL-LYR = 0
               MOVE 0 TO PL-VOL-PCT
           ELSE
               COMPUTE PL-VOL-PCT ROUNDED =
                  PL-VOL-YTD / PL-VOL-LYR * 100.
           MOVE PL-VOL-PCT     TO D1-2-VOL-PCT.
           MOVE PL-ACCTS       TO D1-2-ACCTS.
           MOVE PL-ACCTS-MTD   TO D1-2-ACCTS-MTD.
           MOVE PL-ACCTS-YTD   TO D1-2-ACCTS-YTD.
           MOVE PL-NWACCTS-MTD TO D1-2-NWACTS-MTD.
           MOVE PL-NWACCTS-YTD TO D1-2-NWACTS-YTD.
           MOVE PL-NWACCTS-LYR TO D1-2-NWACTS-LYR.

           IF DETAIL-LINE = SPACES
              GO TO PRINT-SEC2-EXIT.

           MOVE "CL" TO D1-2-CL.
           MOVE BY-CLASS TO D1-2-CLASS.

           PERFORM WRITE-DETAIL-LINE.

       PRINT-SEC2-EXIT.
           EXIT.

      ************************
      *   COLLECTIONS
      ************************
       PRINT-SEC3 SECTION.
      ************************
           MOVE SPACES       TO DETAIL-LINE.
           MOVE COL-OTH-MTD  TO D3-OTH-MTD.
           MOVE COL-INT-MTD  TO D3-INT-MTD.
           MOVE COL-LATE-MTD TO D3-LAT-MTD.
           MOVE COL-PRIN-MTD TO D3-PRI-MTD.
           MOVE COL-DEF-MTD  TO D3-DEF-MTD.
           MOVE COL-PLA-MTD  TO D3-PLA-MTD.
           MOVE COL-PLI-MTD  TO D3-PLI-MTD.
           MOVE COL-RNP-MTD  TO D3-RNP-MTD.
           MOVE COL-TOT-MTD  TO D3-TOT-MTD.
           IF DETAIL-LINE NOT = SPACES
              MOVE "CL"      TO D3-4-CL
              MOVE BY-CLASS  TO D3-4-CLASS
              PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES       TO DETAIL-LINE.
           MOVE COL-OTH-YTD  TO D3-OTH-MTD.
           MOVE COL-INT-YTD  TO D3-INT-MTD.
           MOVE COL-LATE-YTD TO D3-LAT-MTD.
           MOVE COL-PRIN-YTD TO D3-PRI-MTD.
           MOVE COL-DEF-YTD  TO D3-DEF-MTD.
           MOVE COL-PLA-YTD  TO D3-PLA-MTD.
           MOVE COL-PLI-YTD  TO D3-PLI-MTD.
           MOVE COL-RNP-YTD  TO D3-RNP-MTD.
           MOVE COL-TOT-YTD  TO D3-TOT-MTD.
           IF DETAIL-LINE NOT = SPACES
              PERFORM WRITE-DETAIL-LINE.
       PRINT-SEC3-EXIT.
           EXIT.

      ***********************
      *  PRINT REBATES
      ***********************
       PRINT-SEC4 SECTION.
      ***********************
           MOVE SPACES    TO DETAIL-LINE.
           MOVE REB-INT-1 TO D4-INT-MTD.
           MOVE REB-CL-1  TO D4-CL-MTD.
           MOVE REB-AH-1  TO D4-AH-MTD.
           MOVE REB-PP-1  TO D4-PP-MTD.
           MOVE REB-O1-1  TO D4-O1-MTD.
           MOVE REB-O2-1  TO D4-O2-MTD.
           MOVE REB-O3-1  TO D4-O3-MTD.
           MOVE REB-SC-1  TO D4-SC-MTD.
           MOVE REB-MF-1  TO D4-MF-MTD.
           MOVE REB-DF-1  TO D4-DF-MTD.
           MOVE REB-TOT-1 TO D4-TOT-MTD.
           IF DETAIL-LINE NOT = SPACES
              MOVE "CL"       TO D3-4-CL
              MOVE BY-CLASS   TO D3-4-CLASS
              PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES      TO DETAIL-LINE.
           MOVE REB-INT-2   TO D4-INT-MTD.
           MOVE REB-CL-2    TO D4-CL-MTD.
           MOVE REB-AH-2    TO D4-AH-MTD.
           MOVE REB-PP-2    TO D4-PP-MTD.
           MOVE REB-O1-2    TO D4-O1-MTD.
           MOVE REB-O2-2    TO D4-O2-MTD.
           MOVE REB-O3-2    TO D4-O3-MTD.
           MOVE REB-SC-2    TO D4-SC-MTD.
           MOVE REB-MF-2    TO D4-MF-MTD.
           MOVE REB-DF-2    TO D4-DF-MTD.
           MOVE REB-TOT-2   TO D4-TOT-MTD.
           IF DETAIL-LINE NOT = SPACES
              PERFORM WRITE-DETAIL-LINE.
       PRINT-SEC4-EXIT.
           EXIT.

      *************************
      *  PRINT APPLICATIONS
      *************************
       PRINT-SEC5 SECTION.
      *************************
           MOVE SPACES TO DETAIL-LINE.
           MOVE APPL-01 TO D5-TAKEN-MTD.
           MOVE APPL-02 TO D5-TAKEN-YTD.
           MOVE APPL-03 TO D5-TDOWN-MTD.
           MOVE APPL-04 TO D5-TDOWN-YTD.
           MOVE APPL-05 TO D5-NOTAKEN-MTD.
           MOVE APPL-06 TO D5-NOTAKEN-YTD.
           MOVE APPL-07 TO D5-LOANS-MTD.
           MOVE APPL-08 TO D5-LOANS-MPCT.
           MOVE APPL-09 TO D5-LOANS-YTD.
           MOVE APPL-10 TO D5-LOANS-YPCT.
           IF DETAIL-LINE NOT = SPACES
              MOVE "CL" TO D5-CL
              MOVE BY-CLASS TO D5-CLASS
              PERFORM WRITE-DETAIL-LINE.
       PRINT-SEC5-EXIT.
           EXIT.

      **********************
      *  PRINT REPO
      **********************
       PRINT-SEC6 SECTION.
      **********************
           MOVE SPACES TO DETAIL-LINE.
           MOVE REPO-OUTS        TO D1-2-OUTSTAND.
           MOVE REPO-PRIN-MTD    TO D1-2-PRIN-MTD.
           MOVE REPO-PRIN-YTD    TO D1-2-PRIN-YTD.
           MOVE REPO-PRIN-PCT    TO D1-2-PRIN-PCT.
           MOVE REPO-VOL-MTD     TO D1-2-VOL-MTD.
           MOVE REPO-VOL-YTD     TO D1-2-VOL-YTD.
           MOVE REPO-VOL-LYR     TO D1-2-VOL-LYR.
           IF REPO-VOL-LYR = 0
               MOVE 0 TO REPO-VOL-PCT
           ELSE
               COMPUTE REPO-VOL-PCT ROUNDED =
                  REPO-VOL-YTD / REPO-VOL-LYR * 100.
           MOVE REPO-VOL-PCT     TO D1-2-VOL-PCT.
           MOVE REPO-ACCTS       TO D1-2-ACCTS.
           MOVE REPO-ACCTS-MTD   TO D1-2-ACCTS-MTD.
           MOVE REPO-ACCTS-YTD   TO D1-2-ACCTS-YTD.
           MOVE REPO-NWACCTS-MTD TO D1-2-NWACTS-MTD.
           MOVE REPO-NWACCTS-YTD TO D1-2-NWACTS-YTD.
           MOVE REPO-NWACCTS-LYR TO D1-2-NWACTS-LYR.

           IF DETAIL-LINE = SPACES
              GO TO PRINT-SEC6-EXIT.

           IF SECTION-SUB = 6 AND
                  (SOURCECD = "Y" AND K < 4)
               MOVE REPO-ELE(K) TO D1-2-TAG3
           ELSE
               MOVE "CL" TO D1-2-CL
               MOVE BY-CLASS TO D1-2-CLASS.

           PERFORM WRITE-DETAIL-LINE.
           IF SECTION-SUB = 6 AND
                  (SOURCECD = "Y" AND K > 3)
               PERFORM WRITE-DETAIL-LINE.
       PRINT-SEC6-EXIT.
           EXIT.

      ********************************
      *  REPOS
      ********************************
       SETUP-SEC6-SUB-CLASS SECTION.
      ********************************
           MOVE OUTS-OUTS        TO S-O-OUTS.
           MOVE OUTS-PRIN-MTD    TO S-O-PRIN-MTD.
           MOVE OUTS-PRIN-YTD    TO S-O-PRIN-YTD.
           MOVE OUTS-PRIN-PCT    TO S-O-PRIN-PCT.
           MOVE OUTS-VOL-MTD     TO S-O-VOL-MTD.
           MOVE OUTS-VOL-YTD     TO S-O-VOL-YTD.
           MOVE OUTS-VOL-LYR     TO S-O-VOL-LYR.
           MOVE OUTS-VOL-PCT     TO S-O-VOL-PCT.
           MOVE OUTS-ACCTS       TO S-O-ACCTS.
           MOVE OUTS-ACCTS-MTD   TO S-O-ACCTS-MTD.
           MOVE OUTS-ACCTS-YTD   TO S-O-ACCTS-YTD.
           MOVE OUTS-NWACCTS-MTD TO S-O-NWACCTS-MTD.
           MOVE OUTS-NWACCTS-YTD TO S-O-NWACCTS-YTD.
           MOVE OUTS-NWACCTS-LYR TO S-O-NWACCTS-LYR.

           MOVE REPO-OUTS        TO S-R-OUTS.
           MOVE REPO-PRIN-MTD    TO S-R-PRIN-MTD.
           MOVE REPO-PRIN-YTD    TO S-R-PRIN-YTD.
           MOVE REPO-PRIN-PCT    TO S-R-PRIN-PCT.
           MOVE REPO-VOL-MTD     TO S-R-VOL-MTD.
           MOVE REPO-VOL-YTD     TO S-R-VOL-YTD.
           MOVE REPO-VOL-LYR     TO S-R-VOL-LYR.
           MOVE REPO-VOL-PCT     TO S-R-VOL-PCT.
           MOVE REPO-ACCTS       TO S-R-ACCTS.
           MOVE REPO-ACCTS-MTD   TO S-R-ACCTS-MTD.
           MOVE REPO-ACCTS-YTD   TO S-R-ACCTS-YTD.
           MOVE REPO-NWACCTS-MTD TO S-R-NWACCTS-MTD.
           MOVE REPO-NWACCTS-YTD TO S-R-NWACCTS-YTD.
           MOVE REPO-NWACCTS-LYR TO S-R-NWACCTS-LYR.

      ******************************
       SETUP-SEC6-SUB-PART SECTION.
      ******************************
           MOVE OUTS-P-TOTOUTS        TO S-O-OUTS.
           MOVE OUTS-P-TOTPRIN-MTD    TO S-O-PRIN-MTD.
           MOVE OUTS-P-TOTPRIN-YTD    TO S-O-PRIN-YTD.
           MOVE OUTS-P-TOTPRIN-PCT    TO S-O-PRIN-PCT.
           MOVE OUTS-P-TOTVOL-MTD     TO S-O-VOL-MTD.
           MOVE OUTS-P-TOTVOL-YTD     TO S-O-VOL-YTD.
           MOVE OUTS-P-TOTVOL-LYR     TO S-O-VOL-LYR.
           MOVE OUTS-P-TOTVOL-PCT     TO S-O-VOL-PCT.
           MOVE OUTS-P-TOTACCTS       TO S-O-ACCTS.
           MOVE OUTS-P-TOTACCTS-MTD   TO S-O-ACCTS-MTD.
           MOVE OUTS-P-TOTACCTS-YTD   TO S-O-ACCTS-YTD.
           MOVE OUTS-P-TOTNWACCTS-MTD TO S-O-NWACCTS-MTD.
           MOVE OUTS-P-TOTNWACCTS-YTD TO S-O-NWACCTS-YTD.
           MOVE OUTS-P-TOTNWACCTS-LYR TO S-O-NWACCTS-LYR.

           MOVE REPO-P-TOTOUTS        TO S-R-OUTS.
           MOVE REPO-P-TOTPRIN-MTD    TO S-R-PRIN-MTD.
           MOVE REPO-P-TOTPRIN-YTD    TO S-R-PRIN-YTD.
           MOVE REPO-P-TOTPRIN-PCT    TO S-R-PRIN-PCT.
           MOVE REPO-P-TOTVOL-MTD     TO S-R-VOL-MTD.
           MOVE REPO-P-TOTVOL-YTD     TO S-R-VOL-YTD.
           MOVE REPO-P-TOTVOL-LYR     TO S-R-VOL-LYR.
           MOVE REPO-P-TOTVOL-PCT     TO S-R-VOL-PCT.
           MOVE REPO-P-TOTACCTS       TO S-R-ACCTS.
           MOVE REPO-P-TOTACCTS-MTD   TO S-R-ACCTS-MTD.
           MOVE REPO-P-TOTACCTS-YTD   TO S-R-ACCTS-YTD.
           MOVE REPO-P-TOTNWACCTS-MTD TO S-R-NWACCTS-MTD.
           MOVE REPO-P-TOTNWACCTS-YTD TO S-R-NWACCTS-YTD.
           MOVE REPO-P-TOTNWACCTS-LYR TO S-R-NWACCTS-LYR.

      *********************************
       SETUP-SEC6-SUB-SECTION SECTION.
      *********************************
           MOVE OUTS-S-TOTOUTS        TO S-O-OUTS.
           MOVE OUTS-S-TOTPRIN-MTD    TO S-O-PRIN-MTD.
           MOVE OUTS-S-TOTPRIN-YTD    TO S-O-PRIN-YTD.
           MOVE OUTS-S-TOTPRIN-PCT    TO S-O-PRIN-PCT.
           MOVE OUTS-S-TOTVOL-MTD     TO S-O-VOL-MTD.
           MOVE OUTS-S-TOTVOL-YTD     TO S-O-VOL-YTD.
           MOVE OUTS-S-TOTVOL-LYR     TO S-O-VOL-LYR.
           MOVE OUTS-S-TOTVOL-PCT     TO S-O-VOL-PCT.
           MOVE OUTS-S-TOTACCTS       TO S-O-ACCTS.
           MOVE OUTS-S-TOTACCTS-MTD   TO S-O-ACCTS-MTD.
           MOVE OUTS-S-TOTACCTS-YTD   TO S-O-ACCTS-YTD.
           MOVE OUTS-S-TOTNWACCTS-MTD TO S-O-NWACCTS-MTD.
           MOVE OUTS-S-TOTNWACCTS-YTD TO S-O-NWACCTS-YTD.
           MOVE OUTS-S-TOTNWACCTS-LYR TO S-O-NWACCTS-LYR.

           MOVE REPO-S-TOTOUTS        TO S-R-OUTS.
           MOVE REPO-S-TOTPRIN-MTD    TO S-R-PRIN-MTD.
           MOVE REPO-S-TOTPRIN-YTD    TO S-R-PRIN-YTD.
           MOVE REPO-S-TOTPRIN-PCT    TO S-R-PRIN-PCT.
           MOVE REPO-S-TOTVOL-MTD     TO S-R-VOL-MTD.
           MOVE REPO-S-TOTVOL-YTD     TO S-R-VOL-YTD.
           MOVE REPO-S-TOTVOL-LYR     TO S-R-VOL-LYR.
           MOVE REPO-S-TOTVOL-PCT     TO S-R-VOL-PCT.
           MOVE REPO-S-TOTACCTS       TO S-R-ACCTS.
           MOVE REPO-S-TOTACCTS-MTD   TO S-R-ACCTS-MTD.
           MOVE REPO-S-TOTACCTS-YTD   TO S-R-ACCTS-YTD.
           MOVE REPO-S-TOTNWACCTS-MTD TO S-R-NWACCTS-MTD.
           MOVE REPO-S-TOTNWACCTS-YTD TO S-R-NWACCTS-YTD.
           MOVE REPO-S-TOTNWACCTS-LYR TO S-R-NWACCTS-LYR.

      ******************************************************************
      * PRINT REPO SUB TOTALS ON PAGE 1 (OUTSTANDING)
      ******************************************************************
       PRINT-SEC6-SUB SECTION.
           MOVE SPACES          TO DETAIL-LINE.
           MOVE S-R-OUTS        TO D1-2-OUTSTAND.
           MOVE S-R-PRIN-MTD    TO D1-2-PRIN-MTD.
           MOVE S-R-PRIN-YTD    TO D1-2-PRIN-YTD.
      *--->MOVE S-R-PRIN-PCT    TO D1-2-PRIN-PCT.
           MOVE S-R-VOL-MTD     TO D1-2-VOL-MTD.
           MOVE S-R-VOL-YTD     TO D1-2-VOL-YTD.
           MOVE S-R-VOL-LYR     TO D1-2-VOL-LYR.
           IF S-R-VOL-LYR = 0
               MOVE 0 TO S-R-VOL-PCT
           ELSE
               COMPUTE S-R-VOL-PCT ROUNDED =
                       S-R-VOL-YTD / S-R-VOL-LYR * 100.
           MOVE S-R-VOL-PCT     TO D1-2-VOL-PCT.
           MOVE S-R-ACCTS       TO D1-2-ACCTS.
           MOVE S-R-ACCTS-MTD   TO D1-2-ACCTS-MTD.
           MOVE S-R-ACCTS-YTD   TO D1-2-ACCTS-YTD.
           MOVE S-R-NWACCTS-MTD TO D1-2-NWACTS-MTD.
           MOVE S-R-NWACCTS-YTD TO D1-2-NWACTS-YTD.
           MOVE S-R-NWACCTS-LYR TO D1-2-NWACTS-LYR.
           IF DETAIL-LINE = SPACES
              GO TO PRINT-SEC6-SUB-EXIT.

           MOVE "REPO" TO D1-2-SOURCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES              TO DETAIL-LINE.
           IF ( S-O-OUTS + S-R-OUTS ) NOT = 0
              MOVE "-----------"    TO D1-2-OUTSTANDX.
           IF ( S-O-PRIN-MTD + S-R-PRIN-MTD ) NOT = 0
              MOVE "-----------"    TO D1-2-PRIN-MTDX.
           IF ( S-O-PRIN-YTD + S-R-PRIN-YTD ) NOT = 0
              MOVE "-----------"    TO D1-2-PRIN-YTDX.
      *--->IF ( S-O-PRIN-PCT + S-R-PRIN-PCT ) NOT = 0
      *--->   MOVE "------"         TO D1-2-PRIN-PCTX.
           IF ( S-O-VOL-MTD + S-R-VOL-MTD ) NOT = 0
              MOVE "-----------"    TO D1-2-VOL-MTDX.
           IF ( S-O-VOL-YTD + S-R-VOL-YTD ) NOT = 0
              MOVE "-----------"    TO D1-2-VOL-YTDX.
           IF ( S-O-VOL-LYR + S-R-VOL-LYR ) NOT = 0
              MOVE "-----------"    TO D1-2-VOL-LYRX.
      *--->IF ( S-O-VOL-PCT + S-R-VOL-PCT ) NOT = 0
      *--->   MOVE "------"         TO D1-2-VOL-PCTX.
           IF ( S-O-ACCTS + S-R-ACCTS ) NOT = 0
              MOVE "------"         TO D1-2-ACCTSX.
           IF ( S-O-ACCTS-MTD + S-R-ACCTS-MTD ) NOT = 0
              MOVE "-----"          TO D1-2-ACCTS-MTDX.
           IF ( S-O-ACCTS-YTD + S-R-ACCTS-YTD ) NOT = 0
              MOVE "------"         TO D1-2-ACCTS-YTDX.
           IF ( S-O-NWACCTS-MTD + S-R-NWACCTS-MTD ) NOT = 0
              MOVE "------"         TO D1-2-NWACTS-MTDX.
           IF ( S-O-NWACCTS-YTD + S-R-NWACCTS-YTD ) NOT = 0
              MOVE "------"         TO D1-2-NWACTS-YTDX.
           IF ( S-O-NWACCTS-LYR + S-R-NWACCTS-LYR ) NOT = 0
              MOVE "------"         TO D1-2-NWACTS-LYRX.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO DETAIL-LINE.
           ADD S-O-OUTS        S-R-OUTS        GIVING D1-2-OUTSTAND.
           ADD S-O-PRIN-MTD    S-R-PRIN-MTD    GIVING D1-2-PRIN-MTD.
           ADD S-O-PRIN-YTD    S-R-PRIN-YTD    GIVING D1-2-PRIN-YTD.
      *--->ADD S-O-PRIN-PCT    S-R-PRIN-PCT    GIVING D1-2-PRIN-PCT.
           ADD S-O-VOL-MTD     S-R-VOL-MTD     GIVING D1-2-VOL-MTD.
           ADD S-O-VOL-YTD     S-R-VOL-YTD     GIVING D1-2-VOL-YTD.
           ADD S-O-VOL-LYR     S-R-VOL-LYR     GIVING D1-2-VOL-LYR.
           IF (S-O-VOL-LYR + S-R-VOL-YTD) = 0
               MOVE 0 TO S-O-VOL-PCT
           ELSE
               COMPUTE S-O-VOL-PCT ROUNDED =
                                     (S-O-VOL-YTD + S-R-VOL-YTD) /
                                     (S-O-VOL-LYR + S-R-VOL-LYR) * 100.
           MOVE S-O-VOL-PCT     TO D1-2-VOL-PCT.
           ADD S-O-ACCTS       S-R-ACCTS       GIVING D1-2-ACCTS.
           ADD S-O-ACCTS-MTD   S-R-ACCTS-MTD   GIVING D1-2-ACCTS-MTD.
           ADD S-O-ACCTS-YTD   S-R-ACCTS-YTD   GIVING D1-2-ACCTS-YTD.
           ADD S-O-NWACCTS-MTD S-R-NWACCTS-MTD GIVING D1-2-NWACTS-MTD.
           ADD S-O-NWACCTS-YTD S-R-NWACCTS-YTD GIVING D1-2-NWACTS-YTD.
           ADD S-O-NWACCTS-LYR S-R-NWACCTS-LYR GIVING D1-2-NWACTS-LYR.

           MOVE "  TOT:" TO D1-2-TAG3.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-SEC6-SUB-EXIT.
           EXIT.

      **********************************
       CREATE-TOTAL-RECS SECTION.
      **********************************
       CREATE-TOT-RECS.
           IF LC-SALEFIN = "N" AND LC-SERVACCT = "N"
               MOVE 1 TO WK-LOAN-TYPE HOLD-WK-TYPE.
           IF LC-SALEFIN = "Y"
               MOVE 2 TO WK-LOAN-TYPE HOLD-WK-TYPE.
           IF LC-SERVACCT = "Y"
               MOVE 3 TO WK-LOAN-TYPE HOLD-WK-TYPE.

           MOVE BY-CLASS TO WK-CLASS.
           PERFORM READ-WK-FILE.
           IF IO-GOOD
              GO TO UPDATE-WK-RECORD.

       CREATE-WK.
           MOVE HOLD-WK-TYPE TO WK-LOAN-TYPE.
           MOVE BY-CLASS TO WK-CLASS.
           PERFORM CLEAR-WK-FILE.
           PERFORM WRITE-WK-FILE.
           IF IO-BAD
               GO TO IO-ERROR.
           GO TO CREATE-TOT-RECS.

       UPDATE-WK-RECORD.
           PERFORM ADD-PERIOD-TABLES
                   VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                     AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
                     AFTER S3 FROM 1 BY 1 UNTIL S3 > 11.

           PERFORM ADD-MTD-YTD
               VARYING S1 FROM 1 BY 1 UNTIL S1 > 2.

           PERFORM ADD-MISC-FIELDS.

           PERFORM REWRITE-WK-FILE.
           GO TO CREATE-TOTAL-RECS-X.
      *----------------------------------------------------------------
       ADD-PERIOD-TABLES.
              ADD BY-NO(S1, S2, S3) TO WK-NO(S1, S2, S3)
              ADD BY-AMT(S1, S2, S3) TO WK-AMT(S1, S2, S3)
              IF S3 < 11
                 ADD BY-NO-PL(S1 S2 S3) TO WK-NO-PL(S1 S2 S3)
                 ADD BY-AMT-PL(S1 S2 S3) TO WK-AMT-PL(S1 S2 S3)
                 ADD BY-UNITS(S1 S2 S3) TO WK-UNITS(S1 S2 S3)
                 ADD BY-OUTBAL(S1 S2 S3) TO WK-OUTBAL(S1 S2 S3)
              IF S3 < 6
                 ADD BY-NO-REPO(S1 S2 S3) TO WK-NO-REPO(S1 S2 S3)
                 ADD BY-AMT-REPO(S1 S2 S3) TO WK-AMT-REPO(S1 S2 S3).
              IF S3 < 4
                 ADD BY-DLRVPRIN(S1 S2 S3) TO WK-DLRVPRIN(S1 S2 S3)
                 ADD BY-DLRVCHGS(S1 S2 S3) TO WK-DLRVCHGS(S1 S2 S3)
                 IF S3 = 1
                    ADD BY-DLPARVHELD(S1 S2) TO WK-DLPARVHELD(S1 S2).

       ADD-MTD-YTD.
           PERFORM VARYING S4 FROM 1 BY 1 UNTIL S4 > 5
              ADD BY-PL-PRINCOL(S1 S4) TO  WK-PL-PRINCOL(S1 S4)
              ADD BY-PL-INTCOL(S1 S4) TO WK-PL-INTCOL(S1 S4)
              ADD BY-PL-LATECOL(S1 S4) TO WK-PL-LATECOL(S1 S4)
              ADD BY-PL-O2COL(S1 S4) TO WK-PL-O2COL(S1 S4)
           END-PERFORM.

              ADD BY-AMT-SOLD(S1) TO  WK-AMT-SOLD(S1).
              ADD BY-AMT-SOLD(S1) TO  WK-AMT-TRANS(S1).
              ADD BY-AMT-SOLD(S1) TO  WK-AMT-PURCH(S1).

              ADD BY-NO-SOLD(S1) TO  WK-NO-SOLD(S1).
              ADD BY-NO-TRANS(S1) TO  WK-NO-TRANS(S1).
              ADD BY-NO-PURCH(S1) TO  WK-NO-PURCH(S1).

              ADD BY-OTHCOL(S1) TO  WK-OTHCOL(S1).
              ADD BY-OT2COL(S1) TO  WK-OT2COL(S1).
              ADD BY-INTCOL(S1) TO  WK-INTCOL(S1).
              ADD BY-LATECOL(S1) TO  WK-LATECOL(S1).
              ADD BY-PRINCOL(S1) TO  WK-PRINCOL(S1).
              ADD BY-DEFCOL(S1) TO  WK-DEFCOL(S1).
              ADD BY-RNPRIN(S1) TO  WK-RNPRIN(S1).
              ADD BY-INTREB(S1) TO  WK-INTREB(S1).
              ADD BY-CLREB1(S1) TO  WK-CLREB1(S1).
              ADD BY-CLREB2(S1) TO  WK-CLREB2(S1).
              ADD BY-CLREB3(S1) TO  WK-CLREB3(S1).
              ADD BY-CLREB4(S1) TO  WK-CLREB4(S1).
              ADD BY-AHREB(S1) TO  WK-AHREB(S1).
              ADD BY-PPREB(S1) TO  WK-PPREB(S1).
              ADD BY-O1REB(S1) TO  WK-O1REB(S1).
              ADD BY-O2REB(S1) TO  WK-O2REB(S1).
              ADD BY-O3REB(S1) TO  WK-O3REB(S1).
              ADD BY-O4REB(S1) TO  WK-O4REB(S1).
              ADD BY-O5REB(S1) TO  WK-O5REB(S1).
              ADD BY-O6REB(S1) TO  WK-O6REB(S1).
              ADD BY-O7REB(S1) TO  WK-O7REB(S1).
              ADD BY-SCREB(S1) TO  WK-SCREB(S1).
              ADD BY-MFREB(S1) TO  WK-MFREB(S1).
              ADD BY-DFREB(S1) TO  WK-DFREB(S1).
              ADD BY-PART-CHGOFF(S1) TO  WK-PART-CHGOFF(S1).
              ADD BY-ALLOTMENTS(S1) TO  WK-ALLOTMENTS(S1).

              PERFORM VARYING S4 FROM 1 BY 1 UNTIL S4 >10
                 ADD BY-INS-NO-ELIG(S1 S4) TO WK-INS-NO-ELIG(S1 S4)
                 ADD BY-INS-NO-WROTE(S1 S4) TO WK-INS-NO-WROTE(S1 S4)
                 ADD BY-INS-NO-NOTWROTE(S1 S4)
                                           TO WK-INS-NO-NOTWROTE(S1 S4)
                 ADD BY-INS-AMT-WROTE(S1 S4) TO WK-INS-AMT-WROTE(S1 S4)
              END-PERFORM.

              ADD BY-OTH-NO-COL(S1) TO WK-OTH-NO-COL(S1).
              ADD BY-OT2-NO-COL(S1) TO WK-OT2-NO-COL(S1).
              ADD BY-INT-NO-COL(S1) TO WK-INT-NO-COL(S1).
              ADD BY-LATE-NO-COL(S1) TO WK-LATE-NO-COL(S1).

              ADD BY-OTH-NO-WAIVED(S1) TO WK-OTH-NO-WAIVED(S1).
              ADD BY-OT2-NO-WAIVED(S1) TO WK-OT2-NO-WAIVED(S1).
              ADD BY-INT-NO-WAIVED(S1) TO WK-INT-NO-WAIVED(S1).
              ADD BY-LATE-NO-WAIVED(S1) TO WK-LATE-NO-WAIVED(S1).

              ADD BY-OTH-AMT-WAIVED(S1) TO WK-OTH-AMT-WAIVED(S1).
              ADD BY-OT2-AMT-WAIVED(S1) TO WK-OT2-AMT-WAIVED(S1).
              ADD BY-INT-AMT-WAIVED(S1) TO WK-INT-AMT-WAIVED(S1).
              ADD BY-LATE-AMT-WAIVED(S1) TO WK-LATE-AMT-WAIVED(S1).

       ADD-MISC-FIELDS.
              PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                 ADD BY-DLDISC(S1) TO WK-DLDISC(S2)
              END-PERFORM.
              ADD BY-SUMWGTTM TO WK-SUMWGTTM.
              ADD BY-SUMWGTAV TO WK-SUMWGTAV.
              ADD BY-SUMWGTTA TO WK-SUMWGTTA.
              ADD BY-IB-ACCRUED TO WK-IB-ACCRUED.
              ADD BY-STD-POS TO WK-STD-POS.
              ADD BY-30-DAY-MOVED TO WK-30-DAY-MOVED.
              ADD BY-PROJ-INCREASE TO WK-PROJ-INCREASE.
              ADD BY-PROJ-PROFIT TO WK-PROJ-PROFIT.
      *----------------------------------------------------------------
       CREATE-TOTAL-RECS-X.
           EXIT.

      **************************
       PART-TOTALS SECTION.
      **************************
           MOVE TAGNAME(ACCT-TYPE) TO D1-2-TAG3.
           EVALUATE SECTION-SUB
                WHEN 1 PERFORM PART-TOT1
                WHEN 2 PERFORM PART-TOT2
                WHEN 3 PERFORM PART-TOT3
                WHEN 4 PERFORM PART-TOT4
                WHEN 5 PERFORM PART-TOT5
                WHEN 6 PERFORM PART-TOT6
           END-EVALUATE.
       PART-TOTS-EXIT.
           EXIT.

      **********************
       PART-TOT1 SECTION.
      **********************
           IF CORP-TOTALS 
              PERFORM STAGGER-PART-TOTALS
              GO TO PART-TOT1-CONTINUE.

           MOVE OUTS-P-TOTOUTS        TO D1-2-OUTSTAND.
           MOVE OUTS-P-TOTPRIN-MTD    TO D1-2-PRIN-MTD.
           MOVE OUTS-P-TOTPRIN-YTD    TO D1-2-PRIN-YTD.
      *    -------------------------------------------------------------
      *    DONT PRINT PERCENT P-TOTALS!!!!
      *    -------------------------------------------------------------
      *    MOVE OUTS-P-TOTPRIN-PCT    TO D1-2-PRIN-PCT.
           MOVE OUTS-P-TOTVOL-MTD     TO D1-2-VOL-MTD.
           MOVE OUTS-P-TOTVOL-YTD     TO D1-2-VOL-YTD.
           MOVE OUTS-P-TOTVOL-LYR     TO D1-2-VOL-LYR.
      *    -------------------------------------------------------------
      *    DONT PRINT PERCENT P-TOTALS!!!!
      *    -------------------------------------------------------------
           IF OUTS-P-TOTVOL-LYR = 0
               MOVE 0 TO OUTS-P-TOTVOL-PCT
           ELSE
               COMPUTE OUTS-P-TOTVOL-PCT ROUNDED =
                       OUTS-P-TOTVOL-YTD / OUTS-P-TOTVOL-LYR * 100.
           MOVE OUTS-P-TOTVOL-PCT     TO D1-2-VOL-PCT.
           MOVE OUTS-P-TOTACCTS       TO D1-2-ACCTS.
           MOVE OUTS-P-TOTACCTS-MTD   TO D1-2-ACCTS-MTD.
           MOVE OUTS-P-TOTACCTS-YTD   TO D1-2-ACCTS-YTD.
           MOVE OUTS-P-TOTNWACCTS-MTD TO D1-2-NWACTS-MTD.
           MOVE OUTS-P-TOTNWACCTS-YTD TO D1-2-NWACTS-YTD.
           MOVE OUTS-P-TOTNWACCTS-LYR TO D1-2-NWACTS-LYR.
      *     IF DETAIL-LINE NOT = SPACES
              PERFORM WRITE-DETAIL-LINE.



       PART-TOT1-CONTINUE.
           IF INCL-REPO-WITH-OUTS-Y
              PERFORM PART-TOT6
              PERFORM SETUP-SEC6-SUB-PART
              PERFORM PRINT-SEC6-SUB
              PERFORM ZERO-P6-TOTALS.

           MOVE 2 TO LN-FEED.
           ADD OUTS-P-TOTOUTS        TO OUTS-S-TOTOUTS.
           ADD OUTS-P-TOTPRIN-MTD    TO OUTS-S-TOTPRIN-MTD.
           ADD OUTS-P-TOTPRIN-YTD    TO OUTS-S-TOTPRIN-YTD.
           ADD OUTS-P-TOTPRIN-PCT    TO OUTS-S-TOTPRIN-PCT.
           ADD OUTS-P-TOTVOL-MTD     TO OUTS-S-TOTVOL-MTD.
           ADD OUTS-P-TOTVOL-YTD     TO OUTS-S-TOTVOL-YTD.
           ADD OUTS-P-TOTVOL-LYR     TO OUTS-S-TOTVOL-LYR.
           ADD OUTS-P-TOTVOL-PCT     TO OUTS-S-TOTVOL-PCT.
           ADD OUTS-P-TOTACCTS       TO OUTS-S-TOTACCTS.
           ADD OUTS-P-TOTACCTS-MTD   TO OUTS-S-TOTACCTS-MTD.
           ADD OUTS-P-TOTACCTS-YTD   TO OUTS-S-TOTACCTS-YTD.
           ADD OUTS-P-TOTNWACCTS-MTD TO OUTS-S-TOTNWACCTS-MTD.
           ADD OUTS-P-TOTNWACCTS-YTD TO OUTS-S-TOTNWACCTS-YTD.
           ADD OUTS-P-TOTNWACCTS-LYR TO OUTS-S-TOTNWACCTS-LYR.
           PERFORM ZERO-P1-TOTALS.
           IF NOT ((ACCT-TYPE = 1 OR 3) OR MAX-PARTS-PER-SECTION = 2)
              MOVE TAGNAME(4) TO D1-2-TAG3
              PERFORM SEC-TOT1
              MOVE 2 TO LN-FEED.
       PART-TOT1-EXIT.
           EXIT.

      *******************************
       STAGGER-PART-TOTALS SECTION.
      *******************************
           MOVE OUTS-P-TOTPRIN-MTD    TO C1-2-PRIN-MTD.
           MOVE OUTS-P-TOTVOL-MTD     TO C1-2-VOL-MTD.
           MOVE OUTS-P-TOTVOL-LYR     TO C1-2-VOL-LYR.
           IF OUTS-P-TOTVOL-LYR = 0
               MOVE 0 TO OUTS-P-TOTVOL-PCT
           ELSE
               COMPUTE OUTS-P-TOTVOL-PCT ROUNDED =
                       OUTS-P-TOTVOL-YTD / OUTS-P-TOTVOL-LYR * 100.
           MOVE OUTS-P-TOTVOL-PCT     TO C1-2-VOL-PCT.

           MOVE OUTS-P-TOTACCTS       TO C1-2-ACCTS.
           MOVE OUTS-P-TOTACCTS-MTD   TO C1-2-ACCTS-MTD.
           MOVE OUTS-P-TOTACCTS-YTD   TO C1-2-ACCTS-YTD.
           MOVE OUTS-P-TOTNWACCTS-MTD TO C1-2-NWACTS-MTD.
           MOVE OUTS-P-TOTNWACCTS-YTD TO C1-2-NWACTS-YTD.
           MOVE OUTS-P-TOTNWACCTS-LYR TO C1-2-NWACTS-LYR.
           PERFORM WRITE-DETAIL-LINE.

           MOVE OUTS-P-TOTOUTS        TO C1-2-OUTSTAND.
           MOVE OUTS-P-TOTPRIN-YTD    TO C1-2-PRIN-YTD.
           MOVE OUTS-P-TOTVOL-YTD     TO C1-2-VOL-YTD.
           PERFORM WRITE-DETAIL-LINE.
      ********************
       PART-TOT2 SECTION.
      ********************
           MOVE PL-P-TOTOUTS        TO D1-2-OUTSTAND.
           MOVE PL-P-TOTPRIN-MTD    TO D1-2-PRIN-MTD.
           MOVE PL-P-TOTPRIN-YTD    TO D1-2-PRIN-YTD.
      *    -------------------------------------------------------------
      *    DONT PRINT PERCENT P-TOTALS!!!!
      *    -------------------------------------------------------------
      *    MOVE PL-P-TOTPRIN-PCT    TO D1-2-PRIN-PCT.
           MOVE PL-P-TOTVOL-MTD     TO D1-2-VOL-MTD.
           MOVE PL-P-TOTVOL-YTD     TO D1-2-VOL-YTD.
           MOVE PL-P-TOTVOL-LYR     TO D1-2-VOL-LYR.
      *    -------------------------------------------------------------
      *    DONT PRINT PERCENT P-TOTALS!!!!
      *    -------------------------------------------------------------
           IF PL-P-TOTVOL-LYR = 0
               MOVE 0 TO PL-P-TOTVOL-PCT
           ELSE
               COMPUTE PL-P-TOTVOL-PCT ROUNDED =
                  PL-P-TOTVOL-YTD / PL-P-TOTVOL-LYR * 100.
           MOVE PL-P-TOTVOL-PCT     TO D1-2-VOL-PCT.
           MOVE PL-P-TOTACCTS       TO D1-2-ACCTS.
           MOVE PL-P-TOTACCTS-MTD   TO D1-2-ACCTS-MTD.
           MOVE PL-P-TOTACCTS-YTD   TO D1-2-ACCTS-YTD.
           MOVE PL-P-TOTNWACCTS-MTD TO D1-2-NWACTS-MTD.
           MOVE PL-P-TOTNWACCTS-YTD TO D1-2-NWACTS-YTD.
           MOVE PL-P-TOTNWACCTS-LYR TO D1-2-NWACTS-LYR.
      *     IF DETAIL-LINE NOT = SPACES
              PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           ADD PL-P-TOTOUTS        TO PL-S-TOTOUTS.
           ADD PL-P-TOTPRIN-MTD    TO PL-S-TOTPRIN-MTD.
           ADD PL-P-TOTPRIN-YTD    TO PL-S-TOTPRIN-YTD.
           ADD PL-P-TOTPRIN-PCT    TO PL-S-TOTPRIN-PCT.
           ADD PL-P-TOTVOL-MTD     TO PL-S-TOTVOL-MTD.
           ADD PL-P-TOTVOL-YTD     TO PL-S-TOTVOL-YTD.
           ADD PL-P-TOTVOL-LYR     TO PL-S-TOTVOL-LYR.
           ADD PL-P-TOTVOL-PCT     TO PL-S-TOTVOL-PCT.
           ADD PL-P-TOTACCTS       TO PL-S-TOTACCTS.
           ADD PL-P-TOTACCTS-MTD   TO PL-S-TOTACCTS-MTD.
           ADD PL-P-TOTACCTS-YTD   TO PL-S-TOTACCTS-YTD.
           ADD PL-P-TOTNWACCTS-MTD TO PL-S-TOTNWACCTS-MTD.
           ADD PL-P-TOTNWACCTS-YTD TO PL-S-TOTNWACCTS-YTD.
           ADD PL-P-TOTNWACCTS-LYR TO PL-S-TOTNWACCTS-LYR.
           PERFORM ZERO-P2-TOTALS.
           IF NOT ((ACCT-TYPE = 1 OR 3) OR MAX-PARTS-PER-SECTION = 2)
              MOVE TAGNAME(4) TO D1-2-TAG3
              PERFORM SEC-TOT2
              MOVE 2 TO LN-FEED.

      *********************
       PART-TOT3 SECTION.
      *********************
           MOVE COL-P-TOTOTH-MTD TO D3-OTH-MTD.
           MOVE COL-P-TOTINT-MTD TO D3-INT-MTD.
           MOVE COL-P-TOTLATE-MTD TO D3-LAT-MTD.
           MOVE COL-P-TOTPRIN-MTD TO D3-PRI-MTD.
           MOVE COL-P-TOTDEF-MTD TO D3-DEF-MTD.
           MOVE COL-P-TOTPLA-MTD TO D3-PLA-MTD.
           MOVE COL-P-TOTPLI-MTD TO D3-PLI-MTD.
           MOVE COL-P-TOTRNP-MTD TO D3-RNP-MTD.
           MOVE COL-P-TOTTOT-MTD TO D3-TOT-MTD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE COL-P-TOTOTH-YTD TO D3-OTH-MTD.
           MOVE COL-P-TOTINT-YTD TO D3-INT-MTD.
           MOVE COL-P-TOTLATE-YTD TO D3-LAT-MTD.
           MOVE COL-P-TOTPRIN-YTD TO D3-PRI-MTD.
           MOVE COL-P-TOTDEF-YTD TO D3-DEF-MTD.
           MOVE COL-P-TOTPLA-YTD TO D3-PLA-MTD.
           MOVE COL-P-TOTPLI-YTD TO D3-PLI-MTD.
           MOVE COL-P-TOTRNP-YTD TO D3-RNP-MTD.
           MOVE COL-P-TOTTOT-YTD TO D3-TOT-MTD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           ADD COL-P-TOTOTH-MTD TO COL-S-TOTOTH-MTD.
           ADD COL-P-TOTOTH-YTD TO COL-S-TOTOTH-YTD.
           ADD COL-P-TOTINT-MTD TO COL-S-TOTINT-MTD.
           ADD COL-P-TOTINT-YTD TO COL-S-TOTINT-YTD.
           ADD COL-P-TOTLATE-MTD TO COL-S-TOTLATE-MTD.
           ADD COL-P-TOTLATE-YTD TO COL-S-TOTLATE-YTD.
           ADD COL-P-TOTPRIN-MTD TO COL-S-TOTPRIN-MTD.
           ADD COL-P-TOTPRIN-YTD TO COL-S-TOTPRIN-YTD.
           ADD COL-P-TOTDEF-MTD TO COL-S-TOTDEF-MTD.
           ADD COL-P-TOTDEF-YTD TO COL-S-TOTDEF-YTD.
           ADD COL-P-TOTPLA-MTD TO COL-S-TOTPLA-MTD.
           ADD COL-P-TOTPLA-YTD TO COL-S-TOTPLA-YTD.
           ADD COL-P-TOTPLI-MTD TO COL-S-TOTPLI-MTD.
           ADD COL-P-TOTPLI-YTD TO COL-S-TOTPLI-YTD.
           ADD COL-P-TOTRNP-MTD TO COL-S-TOTRNP-MTD.
           ADD COL-P-TOTRNP-YTD TO COL-S-TOTRNP-YTD.
           ADD COL-P-TOTTOT-MTD TO COL-S-TOTTOT-MTD.
           ADD COL-P-TOTTOT-YTD TO COL-S-TOTTOT-YTD.
           PERFORM ZERO-P3-TOTALS.
           IF NOT ((ACCT-TYPE = 1 OR 3) OR MAX-PARTS-PER-SECTION = 2)
              MOVE TAGNAME(4) TO D1-2-TAG3
              PERFORM SEC-TOT3
              MOVE 2 TO LN-FEED.
       PART-TOT3-EXIT.
           EXIT.

      *********************
       PART-TOT4 SECTION.
      *********************
           MOVE REB-P-TOTINT-1 TO D4-INT-MTD.
           MOVE REB-P-TOTCL-1  TO D4-CL-MTD.
           MOVE REB-P-TOTAH-1  TO D4-AH-MTD.
           MOVE REB-P-TOTPP-1  TO D4-PP-MTD.
           MOVE REB-P-TOTO1-1  TO D4-O1-MTD.
           MOVE REB-P-TOTO2-1  TO D4-O2-MTD.
           MOVE REB-P-TOTO3-1  TO D4-O3-MTD.
           MOVE REB-P-TOTSC-1  TO D4-SC-MTD.
           MOVE REB-P-TOTMF-1  TO D4-MF-MTD.
           MOVE REB-P-TOTDF-1  TO D4-DF-MTD.
           MOVE REB-P-TOTTOT-1 TO D4-TOT-MTD.
           PERFORM WRITE-DETAIL-LINE.
           MOVE REB-P-TOTINT-2 TO D4-INT-MTD.
           MOVE REB-P-TOTCL-2  TO D4-CL-MTD.
           MOVE REB-P-TOTAH-2  TO D4-AH-MTD.
           MOVE REB-P-TOTPP-2  TO D4-PP-MTD.
           MOVE REB-P-TOTO1-2  TO D4-O1-MTD.
           MOVE REB-P-TOTO2-2  TO D4-O2-MTD.
           MOVE REB-P-TOTO3-2  TO D4-O3-MTD.
           MOVE REB-P-TOTSC-2  TO D4-SC-MTD.
           MOVE REB-P-TOTMF-2  TO D4-MF-MTD.
           MOVE REB-P-TOTDF-2  TO D4-DF-MTD.
           MOVE REB-P-TOTTOT-2 TO D4-TOT-MTD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           ADD REB-P-TOTINT-1 TO REB-S-TOTINT-1.
           ADD REB-P-TOTINT-2 TO REB-S-TOTINT-2.
           ADD REB-P-TOTCL-1  TO REB-S-TOTCL-1.
           ADD REB-P-TOTCL-2  TO REB-S-TOTCL-2.
           ADD REB-P-TOTAH-1  TO REB-S-TOTAH-1.
           ADD REB-P-TOTAH-2  TO REB-S-TOTAH-2.
           ADD REB-P-TOTPP-1  TO REB-S-TOTPP-1.
           ADD REB-P-TOTPP-2  TO REB-S-TOTPP-2.
           ADD REB-P-TOTO1-1  TO REB-S-TOTO1-1.
           ADD REB-P-TOTO1-2  TO REB-S-TOTO1-2.
           ADD REB-P-TOTO2-1  TO REB-S-TOTO2-1.
           ADD REB-P-TOTO2-2  TO REB-S-TOTO2-2.
           ADD REB-P-TOTO3-1  TO REB-S-TOTO3-1.
           ADD REB-P-TOTO3-2  TO REB-S-TOTO3-2.
           ADD REB-P-TOTSC-1  TO REB-S-TOTSC-1.
           ADD REB-P-TOTSC-2  TO REB-S-TOTSC-2.
           ADD REB-P-TOTMF-1  TO REB-S-TOTMF-1.
           ADD REB-P-TOTMF-2  TO REB-S-TOTMF-2.
           ADD REB-P-TOTDF-1  TO REB-S-TOTDF-1.
           ADD REB-P-TOTDF-2  TO REB-S-TOTDF-2.
           ADD REB-P-TOTTOT-1 TO REB-S-TOTTOT-1.
           ADD REB-P-TOTTOT-2 TO REB-S-TOTTOT-2.
           PERFORM ZERO-P4-TOTALS.
           IF NOT ((ACCT-TYPE = 1 OR 3) OR MAX-PARTS-PER-SECTION = 2)
              MOVE TAGNAME(4) TO D1-2-TAG3
              PERFORM SEC-TOT4
              MOVE 2 TO LN-FEED.
       PART-TOT4-EXIT.
           EXIT.

      **********************
       PART-TOT5 SECTION.
      **********************
           MOVE APPL-P-TOT01 TO D5-TAKEN-MTD.
           MOVE APPL-P-TOT02 TO D5-TAKEN-YTD.
           MOVE APPL-P-TOT03 TO D5-TDOWN-MTD.
           MOVE APPL-P-TOT04 TO D5-TDOWN-YTD.
           MOVE APPL-P-TOT05 TO D5-NOTAKEN-MTD.
           MOVE APPL-P-TOT06 TO D5-NOTAKEN-YTD.
           MOVE APPL-P-TOT07 TO D5-LOANS-MTD.
           MOVE APPL-P-TOT08 TO D5-LOANS-MPCT.
           MOVE APPL-P-TOT09 TO D5-LOANS-YTD.
           MOVE APPL-P-TOT10 TO D5-LOANS-YPCT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           ADD APPL-P-TOT01 TO APPL-S-TOT01.
           ADD APPL-P-TOT02 TO APPL-S-TOT02.
           ADD APPL-P-TOT03 TO APPL-S-TOT03.
           ADD APPL-P-TOT04 TO APPL-S-TOT04.
           ADD APPL-P-TOT05 TO APPL-S-TOT05.
           ADD APPL-P-TOT06 TO APPL-S-TOT06.
           ADD APPL-P-TOT07 TO APPL-S-TOT07.
           ADD APPL-P-TOT08 TO APPL-S-TOT08.
           ADD APPL-P-TOT09 TO APPL-S-TOT09.
           ADD APPL-P-TOT10 TO APPL-S-TOT10.
           PERFORM ZERO-P5-TOTALS.
           IF NOT ((ACCT-TYPE = 1 OR 3) OR MAX-PARTS-PER-SECTION = 2)
              MOVE TAGNAME(4) TO D1-2-TAG3
              PERFORM SEC-TOT5
              MOVE 2 TO LN-FEED.
       PART-TOT5-EXIT.
           EXIT.

      **********************
       PART-TOT6 SECTION.
      **********************
           IF INCL-REPO-WITH-OUTS-Y AND SECTION-SUB = 1
              GO TO PART-TOT6-SKIP-PRINT.

           MOVE REPO-P-TOTOUTS TO D1-2-OUTSTAND.
           MOVE REPO-P-TOTPRIN-MTD TO D1-2-PRIN-MTD.
           MOVE REPO-P-TOTPRIN-YTD TO D1-2-PRIN-YTD.
      * DONT PRINT PERCENT P-TOTALS!!!!
      *    MOVE REPO-P-TOTPRIN-PCT TO D1-2-PRIN-PCT.
           MOVE REPO-P-TOTVOL-MTD TO D1-2-VOL-MTD.
           MOVE REPO-P-TOTVOL-YTD TO D1-2-VOL-YTD.
           MOVE REPO-P-TOTVOL-LYR TO D1-2-VOL-LYR.
      * DONT PRINT PERCENT P-TOTALS!!!!
           IF REPO-P-TOTVOL-LYR = 0
               MOVE 0 TO REPO-P-TOTVOL-PCT
           ELSE
               COMPUTE REPO-P-TOTVOL-PCT ROUNDED =
                  REPO-P-TOTVOL-YTD / REPO-P-TOTVOL-LYR * 100.
           MOVE REPO-P-TOTVOL-PCT TO D1-2-VOL-PCT.
           MOVE REPO-P-TOTACCTS TO D1-2-ACCTS.
           MOVE REPO-P-TOTACCTS-MTD TO D1-2-ACCTS-MTD.
           MOVE REPO-P-TOTACCTS-YTD TO D1-2-ACCTS-YTD.
           MOVE REPO-P-TOTNWACCTS-MTD TO D1-2-NWACTS-MTD.
           MOVE REPO-P-TOTNWACCTS-YTD TO D1-2-NWACTS-YTD.
           MOVE REPO-P-TOTNWACCTS-LYR TO D1-2-NWACTS-LYR.
      *     IF DETAIL-LINE NOT = SPACES
              PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
       PART-TOT6-SKIP-PRINT.
           ADD REPO-P-TOTOUTS TO REPO-S-TOTOUTS.
           ADD REPO-P-TOTPRIN-MTD TO REPO-S-TOTPRIN-MTD.
           ADD REPO-P-TOTPRIN-YTD TO REPO-S-TOTPRIN-YTD.
           ADD REPO-P-TOTPRIN-PCT TO REPO-S-TOTPRIN-PCT.
           ADD REPO-P-TOTVOL-MTD TO REPO-S-TOTVOL-MTD.
           ADD REPO-P-TOTVOL-YTD TO REPO-S-TOTVOL-YTD.
           ADD REPO-P-TOTVOL-LYR TO REPO-S-TOTVOL-LYR.
           ADD REPO-P-TOTVOL-PCT TO REPO-S-TOTVOL-PCT.
           ADD REPO-P-TOTACCTS TO REPO-S-TOTACCTS.
           ADD REPO-P-TOTACCTS-MTD TO REPO-S-TOTACCTS-MTD.
           ADD REPO-P-TOTACCTS-YTD TO REPO-S-TOTACCTS-YTD.
           ADD REPO-P-TOTNWACCTS-MTD TO REPO-S-TOTNWACCTS-MTD.
           ADD REPO-P-TOTNWACCTS-YTD TO REPO-S-TOTNWACCTS-YTD.
           ADD REPO-P-TOTNWACCTS-LYR TO REPO-S-TOTNWACCTS-LYR.
           IF NOT (INCL-REPO-WITH-OUTS-Y AND SECTION-SUB = 1)
              PERFORM ZERO-P6-TOTALS
              IF NOT ((ACCT-TYPE = 1 OR 3) OR MAX-PARTS-PER-SECTION = 2)
                 MOVE TAGNAME(4) TO D1-2-TAG3
                 PERFORM SEC-TOT6
                 MOVE 2 TO LN-FEED.
       PART-TOT6-EXIT.
           EXIT.

      ********************************
       SECTION-TOTALS SECTION.
      ********************************
       SECTION-TOTS.
           MOVE TAGNAME(5) TO D1-2-TAG3.
           EVALUATE SECTION-SUB
                WHEN 1 PERFORM SEC-TOT1
                       PERFORM ZERO-S1-TOTALS
                WHEN 2 PERFORM SEC-TOT2
                       PERFORM ZERO-S2-TOTALS
                WHEN 3 PERFORM SEC-TOT3
                       PERFORM ZERO-S3-TOTALS
                WHEN 4 PERFORM SEC-TOT4
                       PERFORM ZERO-S4-TOTALS
                WHEN 5 PERFORM SEC-TOT5
                       PERFORM ZERO-S5-TOTALS
                WHEN 6 PERFORM SEC-TOT6
                       PERFORM ZERO-S6-TOTALS
           END-EVALUATE.
           MOVE 2 TO LN-FEED.

      *********************
       SEC-TOT1 SECTION.
      *********************
           IF CORP-TOTALS 
              PERFORM STAGGER-SEC-TOTALS
              GO TO SEC-TOT1-CONTINUE.

           MOVE OUTS-S-TOTOUTS        TO D1-2-OUTSTAND.
           MOVE OUTS-S-TOTPRIN-MTD    TO D1-2-PRIN-MTD.
           MOVE OUTS-S-TOTPRIN-YTD    TO D1-2-PRIN-YTD.
      *    -------------------------------------------------------------
      *    DONT PRINT PERCENT S-TOTALS!!!!
      *    -------------------------------------------------------------
      *    MOVE OUTS-S-TOTPRIN-PCT    TO D1-2-PRIN-PCT.
           MOVE OUTS-S-TOTVOL-MTD     TO D1-2-VOL-MTD.
           MOVE OUTS-S-TOTVOL-YTD     TO D1-2-VOL-YTD.
           MOVE OUTS-S-TOTVOL-LYR     TO D1-2-VOL-LYR.
      *    -------------------------------------------------------------
      *    DONT PRINT PERCENT S-TOTALS!!!!
      *    -------------------------------------------------------------
           IF OUTS-S-TOTVOL-LYR = 0
               MOVE 0 TO OUTS-VOL-PCT
           ELSE
               COMPUTE OUTS-S-TOTVOL-PCT ROUNDED =
                  OUTS-S-TOTVOL-YTD / OUTS-S-TOTVOL-LYR * 100.
           MOVE OUTS-S-TOTVOL-PCT     TO D1-2-VOL-PCT.
           MOVE OUTS-S-TOTACCTS       TO D1-2-ACCTS.
           MOVE OUTS-S-TOTACCTS-MTD   TO D1-2-ACCTS-MTD.
           MOVE OUTS-S-TOTACCTS-YTD   TO D1-2-ACCTS-YTD.
           MOVE OUTS-S-TOTNWACCTS-MTD TO D1-2-NWACTS-MTD.
           MOVE OUTS-S-TOTNWACCTS-YTD TO D1-2-NWACTS-YTD.
           MOVE OUTS-S-TOTNWACCTS-LYR TO D1-2-NWACTS-LYR.
           PERFORM WRITE-DETAIL-LINE.


       SEC-TOT1-CONTINUE.
           IF INCL-REPO-WITH-OUTS-Y
              PERFORM SETUP-SEC6-SUB-SECTION
              PERFORM PRINT-SEC6-SUB.

      *******************************
       STAGGER-SEC-TOTALS SECTION.
      *******************************
           MOVE OUTS-S-TOTPRIN-MTD    TO C1-2-PRIN-MTD.
           MOVE OUTS-S-TOTVOL-MTD     TO C1-2-VOL-MTD.
           MOVE OUTS-S-TOTVOL-LYR     TO C1-2-VOL-LYR.
           IF OUTS-S-TOTVOL-LYR = 0
               MOVE 0 TO OUTS-S-TOTVOL-PCT
           ELSE
               COMPUTE OUTS-S-TOTVOL-PCT ROUNDED =
                       OUTS-S-TOTVOL-YTD / OUTS-S-TOTVOL-LYR * 100.
           MOVE OUTS-S-TOTVOL-PCT     TO C1-2-VOL-PCT.

           MOVE OUTS-S-TOTACCTS       TO C1-2-ACCTS.
           MOVE OUTS-S-TOTACCTS-MTD   TO C1-2-ACCTS-MTD.
           MOVE OUTS-S-TOTACCTS-YTD   TO C1-2-ACCTS-YTD.
           MOVE OUTS-S-TOTNWACCTS-MTD TO C1-2-NWACTS-MTD.
           MOVE OUTS-S-TOTNWACCTS-YTD TO C1-2-NWACTS-YTD.
           MOVE OUTS-S-TOTNWACCTS-LYR TO C1-2-NWACTS-LYR.
           PERFORM WRITE-DETAIL-LINE.

           MOVE OUTS-S-TOTOUTS        TO C1-2-OUTSTAND.
           MOVE OUTS-S-TOTPRIN-YTD    TO C1-2-PRIN-YTD.
           MOVE OUTS-S-TOTVOL-YTD     TO C1-2-VOL-YTD.
           PERFORM WRITE-DETAIL-LINE.

      *******************
       SEC-TOT2 SECTION.
      *******************
           MOVE PL-S-TOTOUTS        TO D1-2-OUTSTAND.
           MOVE PL-S-TOTPRIN-MTD    TO D1-2-PRIN-MTD.
           MOVE PL-S-TOTPRIN-YTD    TO D1-2-PRIN-YTD.
      *    -------------------------------------------------------------
      *    DONT PRINT PERCENT S-TOTALS!!!!
      *    -------------------------------------------------------------
      *    MOVE PL-S-TOTPRIN-PCT    TO D1-2-PRIN-PCT.
           MOVE PL-S-TOTVOL-MTD     TO D1-2-VOL-MTD.
           MOVE PL-S-TOTVOL-YTD     TO D1-2-VOL-YTD.
           MOVE PL-S-TOTVOL-LYR     TO D1-2-VOL-LYR.
      *    -------------------------------------------------------------
      *    DONT PRINT PERCENT S-TOTALS!!!!
      *    -------------------------------------------------------------
           IF PL-S-TOTVOL-LYR = 0
               MOVE 0 TO PL-S-TOTVOL-PCT
           ELSE
               COMPUTE PL-S-TOTVOL-PCT ROUNDED =
                  PL-S-TOTVOL-YTD / PL-S-TOTVOL-LYR * 100.
           MOVE PL-S-TOTVOL-PCT     TO D1-2-VOL-PCT.
           MOVE PL-S-TOTACCTS       TO D1-2-ACCTS.
           MOVE PL-S-TOTACCTS-MTD   TO D1-2-ACCTS-MTD.
           MOVE PL-S-TOTACCTS-YTD   TO D1-2-ACCTS-YTD.
           MOVE PL-S-TOTNWACCTS-MTD TO D1-2-NWACTS-MTD.
           MOVE PL-S-TOTNWACCTS-YTD TO D1-2-NWACTS-YTD.
           MOVE PL-S-TOTNWACCTS-LYR TO D1-2-NWACTS-LYR.
           PERFORM WRITE-DETAIL-LINE.

      ********************
       SEC-TOT3 SECTION.
      ********************
           MOVE COL-S-TOTOTH-MTD TO D3-OTH-MTD.
           MOVE COL-S-TOTINT-MTD TO D3-INT-MTD.
           MOVE COL-S-TOTLATE-MTD TO D3-LAT-MTD.
           MOVE COL-S-TOTPRIN-MTD TO D3-PRI-MTD.
           MOVE COL-S-TOTDEF-MTD TO D3-DEF-MTD.
           MOVE COL-S-TOTPLA-MTD TO D3-PLA-MTD.
           MOVE COL-S-TOTPLI-MTD TO D3-PLI-MTD.
           MOVE COL-S-TOTRNP-MTD TO D3-RNP-MTD.
           MOVE COL-S-TOTTOT-MTD TO D3-TOT-MTD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE COL-S-TOTOTH-YTD TO D3-OTH-MTD.
           MOVE COL-S-TOTINT-YTD TO D3-INT-MTD.
           MOVE COL-S-TOTLATE-YTD TO D3-LAT-MTD.
           MOVE COL-S-TOTPRIN-YTD TO D3-PRI-MTD.
           MOVE COL-S-TOTDEF-YTD TO D3-DEF-MTD.
           MOVE COL-S-TOTPLA-YTD TO D3-PLA-MTD.
           MOVE COL-S-TOTPLI-YTD TO D3-PLI-MTD.
           MOVE COL-S-TOTRNP-YTD TO D3-RNP-MTD.
           MOVE COL-S-TOTTOT-YTD TO D3-TOT-MTD.
           PERFORM WRITE-DETAIL-LINE.

      ********************
       SEC-TOT4 SECTION.
      ********************
           MOVE REB-S-TOTINT-1 TO D4-INT-MTD.
           MOVE REB-S-TOTCL-1  TO D4-CL-MTD.
           MOVE REB-S-TOTAH-1  TO D4-AH-MTD.
           MOVE REB-S-TOTPP-1  TO D4-PP-MTD.
           MOVE REB-S-TOTO1-1  TO D4-O1-MTD.
           MOVE REB-S-TOTO2-1  TO D4-O2-MTD.
           MOVE REB-S-TOTO3-1  TO D4-O3-MTD.
           MOVE REB-S-TOTSC-1  TO D4-SC-MTD.
           MOVE REB-S-TOTMF-1  TO D4-MF-MTD.
           MOVE REB-S-TOTDF-1  TO D4-DF-MTD.
           MOVE REB-S-TOTTOT-1 TO D4-TOT-MTD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE REB-S-TOTINT-2 TO D4-INT-MTD.
           MOVE REB-S-TOTCL-2  TO D4-CL-MTD.
           MOVE REB-S-TOTAH-2  TO D4-AH-MTD.
           MOVE REB-S-TOTPP-2  TO D4-PP-MTD.
           MOVE REB-S-TOTO1-2  TO D4-O1-MTD.
           MOVE REB-S-TOTO2-2  TO D4-O2-MTD.
           MOVE REB-S-TOTO3-2  TO D4-O3-MTD.
           MOVE REB-S-TOTSC-2  TO D4-SC-MTD.
           MOVE REB-S-TOTMF-2  TO D4-MF-MTD.
           MOVE REB-S-TOTDF-2  TO D4-DF-MTD.
           MOVE REB-S-TOTTOT-2 TO D4-TOT-MTD.
           PERFORM WRITE-DETAIL-LINE.

      *******************
       SEC-TOT5 SECTION.
      *******************
           MOVE APPL-S-TOT01 TO D5-TAKEN-MTD.
           MOVE APPL-S-TOT02 TO D5-TAKEN-YTD.
           MOVE APPL-S-TOT03 TO D5-TDOWN-MTD.
           MOVE APPL-S-TOT04 TO D5-TDOWN-YTD.
           MOVE APPL-S-TOT05 TO D5-NOTAKEN-MTD.
           MOVE APPL-S-TOT06 TO D5-NOTAKEN-YTD.
           MOVE APPL-S-TOT07 TO D5-LOANS-MTD.
           MOVE APPL-S-TOT08 TO D5-LOANS-MPCT.
           MOVE APPL-S-TOT09 TO D5-LOANS-YTD.
           MOVE APPL-S-TOT10 TO D5-LOANS-YPCT.
           PERFORM WRITE-DETAIL-LINE.

      *******************
       SEC-TOT6 SECTION.
      *******************
           MOVE REPO-S-TOTOUTS TO D1-2-OUTSTAND.
           MOVE REPO-S-TOTPRIN-MTD TO D1-2-PRIN-MTD.
           MOVE REPO-S-TOTPRIN-YTD TO D1-2-PRIN-YTD.
      * DONT PRINT PERCENT S-TOTALS!!!!
      *    MOVE REPO-S-TOTPRIN-PCT TO D1-2-PRIN-PCT.
           MOVE REPO-S-TOTVOL-MTD TO D1-2-VOL-MTD.
           MOVE REPO-S-TOTVOL-YTD TO D1-2-VOL-YTD.
           MOVE REPO-S-TOTVOL-LYR TO D1-2-VOL-LYR.
      * DONT PRINT PERCENT S-TOTALS!!!!
           IF REPO-S-TOTVOL-LYR = 0
               MOVE 0 TO REPO-S-TOTVOL-PCT
           ELSE
               COMPUTE REPO-S-TOTVOL-PCT ROUNDED =
                  REPO-S-TOTVOL-YTD / REPO-S-TOTVOL-LYR * 100.
           MOVE REPO-S-TOTVOL-PCT TO D1-2-VOL-PCT.
           MOVE REPO-S-TOTACCTS  TO D1-2-ACCTS.
           MOVE REPO-S-TOTACCTS-MTD TO D1-2-ACCTS-MTD.
           MOVE REPO-S-TOTACCTS-YTD TO D1-2-ACCTS-YTD.
           MOVE REPO-S-TOTNWACCTS-MTD TO D1-2-NWACTS-MTD.
           MOVE REPO-S-TOTNWACCTS-YTD TO D1-2-NWACTS-YTD.
           MOVE REPO-S-TOTNWACCTS-LYR TO D1-2-NWACTS-LYR.
           PERFORM WRITE-DETAIL-LINE.

      *************************
       GRAND-TOTALS SECTION.
      *************************

           MOVE 99 TO LN-FEED.
           MOVE "X" TO CORPORATE-FLAG.
           MOVE 0 TO HOLD-BRANCH.
           MOVE 0 TO H-BRNO.
           MOVE "ALL" TO H-BRNAME.

           MOVE FIRST-PART TO ACCT-TYPE.
           MOVE FIRST-SECTION TO SECTION-SUB.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

       BEGIN-WK-FILE.
           MOVE SPACES TO E-O-FLAG.
           MOVE FIRST-PART TO WK-LOAN-TYPE.
           MOVE 0 TO WK-CLASS.
           PERFORM START-WK-FILE.
       BEGIN-WK-X.

       NEXT-WK.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
               MOVE "X" TO E-O-FLAG
               GO TO TEST-GT-END.

           IF WK-LOAN-TYPE > ACCT-TYPE
               GO TO TEST-GT-END.

           IF WK-LOAN-TYPE = ACCT-TYPE
               NEXT SENTENCE
           ELSE
               GO TO NEXT-WK.

       W-CONTINUE.
           IF WK-LOAN-TYPE > ACCT-TYPE
              PERFORM ZERO-P1-TOTALS
              PERFORM ZERO-P2-TOTALS
              PERFORM ZERO-P3-TOTALS
              PERFORM ZERO-P4-TOTALS
              PERFORM ZERO-P5-TOTALS
              PERFORM ZERO-P6-TOTALS
               GO TO TEST-GT-END.
           MOVE WK-CLASS TO BY-CLASS.
           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
              MOVE WK-PERIOD-TBL(S1)      TO BY-PERIOD-TBL(S1)
              MOVE WK-REPO-PERIOD-TBL(S1) TO BY-REPO-PERIOD-TBL(S1)
              MOVE WK-PL-PERIOD-TBL(S1)   TO BY-PL-PERIOD-TBL(S1)
              MOVE WK-DLR-PERIOD-TBL(S1)  TO BY-DLR-PERIOD-TBL(S1)
              IF S1 < 3
                 MOVE WK-MTD-YTD(S1)      TO BY-MTD-YTD(S1)
              END-IF
           END-PERFORM.

       PROCESS-WK.
           PERFORM BY1-DETAIL.
           GO TO NEXT-WK.
      /
       TEST-GT-END.
           PERFORM PART-TOTALS.
           ADD 1 TO ACCT-TYPE.
           IF ACCT-TYPE NOT > MAX-PARTS-PER-SECTION
               IF E-O-F
                  PERFORM ZERO-P1-TOTALS
                  PERFORM ZERO-P2-TOTALS
                  PERFORM ZERO-P3-TOTALS
                  PERFORM ZERO-P4-TOTALS
                  PERFORM ZERO-P5-TOTALS
                  PERFORM ZERO-P6-TOTALS
                  GO TO TEST-GT-END
               ELSE
                  GO TO W-CONTINUE.

           PERFORM SECTION-TOTALS.
           PERFORM ZERO-TOTALS.
           MOVE FIRST-PART TO ACCT-TYPE, WK-LOAN-TYPE.

           ADD 1 TO SECTION-SUB.
           IF SECTION-SUB = 2 AND PL-ISO = "S"
               ADD 1 TO SECTION-SUB.

           IF SECTION-SUB = 6 AND REPO-ISO = "S"
               ADD 1 TO SECTION-SUB.

           IF SECTION-SUB NOT > MAX-SECTIONS-PER-BR
               PERFORM BEGIN-WK-FILE
               PERFORM HEAD-PAGE
               GO TO NEXT-WK.
       GRAND-TOTALS-EXIT.
           EXIT.

      *********************************
       WRITE-DETAIL-LINE SECTION.
      *********************************
       WRITE-DET-LINE.

           IF SUMMARY-ONLY = "Y" AND
                     NOT CORP-TOTALS
               GO TO WRITE-DETAIL-EXIT.
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC..
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
       WRITE-DETAIL-EXIT.
           EXIT.

      *****************************
       HEAD-PAGE SECTION.
      *****************************
       HEAD-ROUTINE.
           MOVE "N" TO FIRST-TIME.
           IF SUMMARY-ONLY = "Y"
               AND NOT CORP-TOTALS
                   GO TO END-HEAD-PAGE.

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.

           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-CONDITIONAL.

           EVALUATE SECTION-SUB
               WHEN 1 PERFORM LOANS-HEADING
               WHEN 2 PERFORM PL-HEADING
               WHEN 3 PERFORM COLLECTIONS-HEADING
               WHEN 4 PERFORM REBATE-HEADING
               WHEN 5 PERFORM APPLICATION-HEADING
               WHEN 6 PERFORM REPO-HEADING
           END-EVALUATE.
           MOVE 7 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      *******************************
      * SECTION 1 HEADINGS
      *******************************
       LOANS-HEADING SECTION.
      *******************************
           IF CORP-TOTALS
              MOVE "CORPORATE:" TO SECTION-TITLE
           ELSE
              MOVE SPACES TO SECTION-TITLE.
           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.

           MOVE HEAD5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.

           MOVE HEAD6 TO PR-REC.
           PERFORM WRITE-PR-CONDITIONAL.

      **********************************
      * SECTION 2 HEADINGS
      **********************************
       PL-HEADING SECTION.
      **********************************
           IF SECTION-SUB = 2
               MOVE "P&L ANALYSIS:" TO SECTION-TITLE
           ELSE
               IF CORP-TOTALS
                   MOVE "CORPORATE:" TO SECTION-TITLE
               ELSE
                   MOVE SPACES TO SECTION-TITLE.
           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.

           MOVE HEAD5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.

           MOVE HEAD6 TO PR-REC.
           PERFORM WRITE-PR-CONDITIONAL.

      ************************************
      * SECTION 3 HEADINGS
      ************************************
       COLLECTIONS-HEADING SECTION.
      ************************************
           MOVE "COLLECTIONS:" TO HEAD-12-TAG.
           MOVE HEAD12 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.
           MOVE HEAD7 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.
           MOVE HEAD9A TO PR-REC.
           PERFORM WRITE-PR-CONDITIONAL.

      **********************************
      * SECTION 4 HEADINGS
      **********************************
       REBATE-HEADING SECTION.
      **********************************
           MOVE "REBATES:" TO HEAD-12-TAG.
           MOVE HEAD12 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.
           MOVE HEAD8 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.
           MOVE HEAD9B TO PR-REC.
           PERFORM WRITE-PR-CONDITIONAL.

      *************************************
      * HEADINGS FOR SECTION 5
      *************************************
       APPLICATION-HEADING SECTION.
      *************************************
           MOVE "APPLICATIONS:" TO HEAD-12-TAG.
           MOVE HEAD12 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.
           MOVE HEAD10 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.
           MOVE HEAD11 TO PR-REC.
           PERFORM WRITE-PR-CONDITIONAL.

      ************************************
      * HEADINGS FOR SECTION 6
      ************************************
       REPO-HEADING SECTION.
      ************************************
           MOVE "REPOSSESSION:" TO SECTION-TITLE.
           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.

           MOVE HEAD5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.

           MOVE HEAD6 TO PR-REC.
           PERFORM WRITE-PR-CONDITIONAL.

      ************************
       RANGE-LINES SECTION.
      ************************
           MOVE 99 TO LN-FEED.
           MOVE "STATISTICS MO/YR" TO RANGE-LINE.
           MOVE MONTH-DATE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           IF GROUP-FLAG = "Y"
              MOVE "GROUP ID" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SEL
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE SAVE-BEG-BR TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE SOURCE CODE DETAIL?" TO RANGE-LINE.
           MOVE SOURCECD TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE REPO SUBTOTALS  ?" TO RANGE-LINE.
           MOVE INCL-REPO-WITH-OUTS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE SERVICE ACCOUNTS?" TO RANGE-LINE.
           MOVE SRVCACCT TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE P & L INFORMATION?" TO RANGE-LINE.
           MOVE PL-ISO TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE REPO INFORMATION?" TO RANGE-LINE.
           MOVE REPO-ISO TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SUMMARY ONLY ?" TO RANGE-LINE.
           MOVE SUMMARY-ONLY TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

      *******************************
       WRITE-PR-CONDITIONAL SECTION.
      *******************************
       WRITE-PR-CHECK.
           IF SUMMARY-ONLY = "Y" AND NOT CORP-TOTALS
              GO TO WRITE-PR-COND-EXIT.
           PERFORM WRITE-PR-REC.
       WRITE-PR-COND-EXIT.
           EXIT.

      *****************************
       CLASS-SCAN SECTION.
      *****************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           MOVE "SNNN020CL1." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ***********************
       GET-TOT-PL SECTION.
      ***********************
           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 3
                     AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 2
               MOVE 0 TO TOT-NO-PLA(SUB1 SUB2)
                         TOT-AMT-PLA(SUB1 SUB2)
                         TOT-NO-PLI(SUB1 SUB2)
                         TOT-AMT-PLI(SUB1 SUB2)
           END-PERFORM.

      *    ACCUMULATE ALL P&L ACTIVE:
           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 3
                     AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 2
              PERFORM VARYING SUB3 FROM 1 BY 2 UNTIL SUB3 > 9
                ADD BY-NO-PL(SUB1 SUB2 SUB3) TO TOT-NO-PLA(SUB1 SUB2)
                ADD BY-AMT-PL(SUB1 SUB2 SUB3) TO TOT-AMT-PLA(SUB1 SUB2)
              END-PERFORM
           END-PERFORM.

      * ACCUMULATE P&L INACTIVE, GAIN-LOSS NOT VOLUME:
           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 3
              PERFORM VARYING SUB3 FROM 2 BY 2 UNTIL SUB3 > 10
                ADD BY-NO-PL(SUB1 1 SUB3) TO TOT-NO-PLA(SUB1 SUB2)
                ADD BY-AMT-PL(SUB1 1 SUB3) TO TOT-AMT-PLA(SUB1 SUB2)
              END-PERFORM
           END-PERFORM.

      * WORLD USES P2'S FOR NONFILE, SO SUBTRACT THEM OUT:
           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 3
                   AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 2
              SUBTRACT BY-NO-PL(SUB1,SUB2,3) FROM
                       TOT-NO-PLA(SUB1,SUB2)
              SUBTRACT BY-AMT-PL(SUB1,SUB2,3) FROM
                       TOT-AMT-PLA(SUB1,SUB2)
           END-PERFORM.

       EXIT-GET-TOT-PL.
           EXIT.

      ***********************
       CLEAR-WK-FILE SECTION.
      ***********************
           PERFORM CLEAR-WK-PL-RECOVERY
                     VARYING S1 FROM 1 BY 1 UNTIL S1 > 5.

           MOVE 0 TO WK-NO-SOLD(1) WK-AMT-SOLD(1)
                     WK-NO-TRANS(1) WK-AMT-TRANS(1)
                     WK-NO-PURCH(1) WK-AMT-PURCH(1)
                     WK-OTHCOL(1)   WK-OT2COL(1)
                     WK-INTCOL(1)   WK-LATECOL(1)  WK-PRINCOL(1)
                     WK-DEFCOL(1)   WK-RNPRIN(1)   WK-INTREB(1)
                     WK-CLREB1(1)   WK-CLREB2(1)   WK-CLREB3(1)
                     WK-CLREB4(1)   WK-AHREB(1)    WK-PPREB(1)
                     WK-O1REB(1)    WK-O2REB(1)    WK-O3REB(1)
                     WK-O4REB(1)    WK-O5REB(1)
                     WK-O6REB(1)    WK-O7REB(1)    WK-SCREB(1)
                     WK-MFREB(1)    WK-DFREB(1)    WK-PART-CHGOFF(1)
                     WK-ALLOTMENTS(1).

           MOVE 0 TO WK-INS-NO-ELIG(1 1)
                     WK-INS-NO-WROTE(1 1)
                     WK-INS-NO-NOTWROTE(1 1)
                     WK-INS-AMT-WROTE(1 1).

           MOVE WK-INS-TYPE(1 1) TO WK-INS-TYPE(1 2).
           MOVE WK-INS-TYPE(1 1) TO WK-INS-TYPE(1 3).
           MOVE WK-INS-TYPE(1 1) TO WK-INS-TYPE(1 4).
           MOVE WK-INS-TYPE(1 1) TO WK-INS-TYPE(1 5).
           MOVE WK-INS-TYPE(1 1) TO WK-INS-TYPE(1 6).
           MOVE WK-INS-TYPE(1 1) TO WK-INS-TYPE(1 7).
           MOVE WK-INS-TYPE(1 1) TO WK-INS-TYPE(1 8).
           MOVE WK-INS-TYPE(1 1) TO WK-INS-TYPE(1 9).
           MOVE WK-INS-TYPE(1 1) TO WK-INS-TYPE(1 10).

           MOVE 0 TO WK-OTH-NO-COL(1)
                     WK-OT2-NO-COL(1)
                     WK-INT-NO-COL(1)
                     WK-LATE-NO-COL(1).

           MOVE 0 TO WK-OTH-NO-WAIVED(1)
                     WK-OT2-NO-WAIVED(1)
                     WK-INT-NO-WAIVED(1)
                     WK-LATE-NO-WAIVED(1).

           MOVE 0 TO WK-OTH-AMT-WAIVED(1)
                     WK-OT2-AMT-WAIVED(1)
                     WK-INT-AMT-WAIVED(1)
                     WK-LATE-AMT-WAIVED(1).

           MOVE WK-MTD-YTD(1) TO WK-MTD-YTD(2).

           MOVE 0 TO WK-PROJ-INCREASE  WK-STD-POS
                     WK-PROJ-PROFIT WK-30-DAY-MOVED.

      * WK-PERIOD-TBL:
           PERFORM CLEAR-WK-PERIOD-TBL VARYING
               S3 FROM 1 BY 1 UNTIL S3 > 11
                  AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
                    AFTER S1 FROM 1 BY 1 UNTIL S1 > 3.

      * WK-DEL-BUCKETS:
           PERFORM CLEAR-WK-DEL-BUCKETS VARYING
               S3 FROM 1 BY 1 UNTIL S3 > 10
                  AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
                    AFTER S1 FROM 1 BY 1 UNTIL S1 > 3.

      * WK-REPO-ACTIVITY-TBL:
           PERFORM CLEAR-WK-REPO-ACTIVITY-TBL VARYING
               S3 FROM 1 BY 1 UNTIL S3 > 5
                  AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
                    AFTER S1 FROM 1 BY 1 UNTIL S1 > 3.

      * WK-PL-ACTIVITY-TBL:
           PERFORM CLEAR-WK-PL-ACTIVITY-TBL VARYING
               S3 FROM 1 BY 1 UNTIL S3 > 10
                  AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
                    AFTER S1 FROM 1 BY 1 UNTIL S1 > 3.

      * WK-DLR-ACTIVITY-TBL:
           PERFORM CLEAR-WK-DLR-ACTIVITY-TBL VARYING
               S3 FROM 1 BY 1 UNTIL S3 > 3
                  AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
                    AFTER S1 FROM 1 BY 1 UNTIL S1 > 3.

      * MISC:
           MOVE 0 TO WK-SUMWGTTM
                     WK-SUMWGTAV
                     WK-SUMWGTTA.

           MOVE 0 TO WK-IB-ACCRUED.

           GO TO CLEAR-WK-FILE-EXIT.


       CLEAR-WK-PL-RECOVERY.
           MOVE 0 TO WK-PL-PRINCOL(1 S1)
                     WK-PL-INTCOL(1 S1)
                     WK-PL-LATECOL(1 S1)
                     WK-PL-O2COL(1 S1).

       CLEAR-WK-PERIOD-TBL.
           MOVE 0 TO WK-NO(S1 S2 S3)
                     WK-AMT(S1 S2 S3)
                     WK-O2(S1 S2 S3)
                     WK-INT(S1 S2 S3).

       CLEAR-WK-DEL-BUCKETS.
           MOVE 0 TO WK-UNITS(S1 S2 S3)
                     WK-OUTBAL(S1 S2 S3).

       CLEAR-WK-REPO-ACTIVITY-TBL.
           MOVE 0 TO WK-NO-REPO(S1 S2 S3)
                     WK-AMT-REPO(S1 S2 S3)
                     WK-O2-REPO(S1 S2 S3)
                     WK-INT-REPO(S1 S2 S3).

       CLEAR-WK-PL-ACTIVITY-TBL.
           MOVE 0 TO WK-NO-PL(S1 S2 S3)
                     WK-AMT-PL(S1 S2 S3)
                     WK-O2-PL(S1 S2 S3)
                     WK-INT-PL(S1 S2 S3).

       CLEAR-WK-DLR-ACTIVITY-TBL.
           MOVE 0 TO WK-DLRVPRIN(S1 S2 S3)
                     WK-DLRVCHGS(S1 S2 S3)
                     WK-DLPARVHELD(S1 S2)
                     WK-DLDISC(S1).

       CLEAR-WK-FILE-EXIT.
           EXIT.

      *********************************
       CLEAR-SECTION-WORKERS SECTION.
      *********************************
           EVALUATE SECTION-SUB
                WHEN 1 PERFORM CLEAR-SEC-1
                WHEN 2 PERFORM CLEAR-SEC-2
                WHEN 3 PERFORM CLEAR-SEC-3
                WHEN 4 PERFORM CLEAR-SEC-4
                WHEN 5 PERFORM CLEAR-SEC-5
                WHEN 6 PERFORM CLEAR-SEC-6
           END-EVALUATE.
       EXIT-CLEAR-SECTION-WORKERS.
           EXIT.

      *********************************
       CLEAR-SEC-1 SECTION.
      *********************************
           MOVE 0 TO OUTS-OUTS        OUTS-PRIN-MTD
                     OUTS-PRIN-YTD    OUTS-PRIN-PCT
                     OUTS-VOL-MTD     OUTS-VOL-YTD
                     OUTS-VOL-LYR     OUTS-VOL-PCT
                     OUTS-ACCTS       OUTS-ACCTS-MTD
                     OUTS-ACCTS-YTD   OUTS-NWACCTS-MTD
                     OUTS-NWACCTS-YTD OUTS-NWACCTS-LYR.

      *********************************
       CLEAR-SEC-2 SECTION.
      *********************************
           MOVE 0 TO PL-OUTS
                     PL-PRIN-MTD  PL-PRIN-YTD PL-PRIN-PCT
                     PL-VOL-MTD PL-VOL-YTD PL-VOL-LYR PL-VOL-PCT
                     PL-ACCTS PL-ACCTS-MTD PL-ACCTS-YTD
                     PL-NWACCTS-MTD PL-NWACCTS-YTD PL-NWACCTS-LYR.

      *********************************
       CLEAR-SEC-3 SECTION.
      *********************************
           MOVE 0 TO COL-OTH-MTD   COL-OTH-YTD
                     COL-INT-MTD   COL-INT-YTD
                     COL-LATE-MTD  COL-LATE-YTD
                     COL-PRIN-MTD  COL-PRIN-YTD
                     COL-DEF-MTD   COL-DEF-YTD
                     COL-PLA-MTD   COL-PLA-YTD
                     COL-PLI-MTD   COL-PLI-YTD
                     COL-RNP-MTD   COL-RNP-YTD
                     COL-TOT-MTD   COL-TOT-YTD.

      *********************************
       CLEAR-SEC-4 SECTION.
      *********************************
           MOVE 0 TO REB-INT-1 REB-INT-2 REB-CL-1   REB-CL-2
                     REB-AH-1  REB-AH-2  REB-PP-1   REB-PP-2
                     REB-O1-1  REB-O1-2  REB-O2-1   REB-O2-2
                     REB-O3-1  REB-O3-2  REB-SC-1   REB-SC-2
                     REB-MF-1  REB-MF-2  REB-DF-1   REB-DF-2
                     REB-TOT-1 REB-TOT-2.

      *********************************
       CLEAR-SEC-5 SECTION.
      *********************************
           MOVE 0 TO APPL-01 APPL-02 APPL-03 APPL-04 APPL-05
                     APPL-06 APPL-07 APPL-08 APPL-09 APPL-10.

      *********************************
       CLEAR-SEC-6 SECTION.
      *********************************
           MOVE 0 TO REPO-OUTS        REPO-PRIN-MTD    REPO-PRIN-YTD
                     REPO-PRIN-PCT    REPO-VOL-MTD     REPO-VOL-YTD
                     REPO-VOL-LYR     REPO-VOL-PCT     REPO-ACCTS
                     REPO-ACCTS-MTD   REPO-ACCTS-YTD   REPO-NWACCTS-MTD
                     REPO-NWACCTS-YTD REPO-NWACCTS-LYR.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      **************************************************************************
      * REQUIRED SECTION: CALLED BY SCREENIO:
      *
      * RESET FORM (SCREEN) TO ITS INITIAL STATE (LIKE FRESET).
      **************************************************************************
       FORM-RESET SECTION.
           MOVE EXT-CONAME-CONAME TO BYANRP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BYANRP-SYSDATE.
           DISPLAY BYANRP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BYANRP-FORM-FIELDS TO WR-BUF.
           MOVE BYANRP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      **************************************************************************
      * REQUIRED SECTION: CALLED BY SCREENIO:
      *
      * SETUP CORRECT HELP FIELDS
      **************************************************************************
       SETUP-LINK-INFO SECTION.
           MOVE BYANRP-HELP--FILE TO HELP-LINK-FILE.
           MOVE BYANRP-HELP--DIR TO HELP-LINK-DIR.

       VDU-CONVERT-FIELD SECTION.
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BYANRP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
      *    M I S C   P A R A G R A P H S
      *****************************************************************
       MISC-PARA SECTION.
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETPR.CPY".
      *----------------------------------------------------------------
       ZERO-S1-TOTALS.
           MOVE 0 TO OUTS-S-TOTOUTS        OUTS-S-TOTPRIN-MTD
                     OUTS-S-TOTPRIN-YTD    OUTS-S-TOTPRIN-PCT
                     OUTS-S-TOTVOL-MTD     OUTS-S-TOTVOL-YTD
                     OUTS-S-TOTVOL-LYR     OUTS-S-TOTVOL-PCT
                     OUTS-S-TOTACCTS       OUTS-S-TOTACCTS-MTD
                     OUTS-S-TOTACCTS-YTD   OUTS-S-TOTNWACCTS-MTD
                     OUTS-S-TOTNWACCTS-YTD OUTS-S-TOTNWACCTS-LYR.
      *----------------------------------------------------------------
       ZERO-S2-TOTALS.
           MOVE 0 TO PL-S-TOTOUTS        PL-S-TOTPRIN-MTD
                     PL-S-TOTPRIN-YTD    PL-S-TOTPRIN-PCT
                     PL-S-TOTVOL-MTD     PL-S-TOTVOL-YTD
                     PL-S-TOTVOL-LYR     PL-S-TOTVOL-PCT
                     PL-S-TOTACCTS       PL-S-TOTACCTS-MTD
                     PL-S-TOTACCTS-YTD   PL-S-TOTNWACCTS-MTD
                     PL-S-TOTNWACCTS-YTD PL-S-TOTNWACCTS-LYR.
      *----------------------------------------------------------------
       ZERO-S3-TOTALS.
           MOVE 0 TO COL-S-TOTOTH-MTD  COL-S-TOTOTH-YTD
                     COL-S-TOTINT-MTD  COL-S-TOTINT-YTD
                     COL-S-TOTLATE-MTD COL-S-TOTLATE-YTD
                     COL-S-TOTPRIN-MTD COL-S-TOTPRIN-YTD
                     COL-S-TOTDEF-MTD  COL-S-TOTDEF-YTD
                     COL-S-TOTPLA-MTD  COL-S-TOTPLA-YTD
                     COL-S-TOTPLI-MTD  COL-S-TOTPLI-YTD
                     COL-S-TOTRNP-MTD  COL-S-TOTRNP-YTD
                     COL-S-TOTTOT-MTD  COL-S-TOTTOT-YTD.
      *----------------------------------------------------------------
       ZERO-S4-TOTALS.
           MOVE 0 TO REB-S-TOTINT-1 REB-S-TOTINT-2 REB-S-TOTCL-1
                     REB-S-TOTCL-2  REB-S-TOTAH-1  REB-S-TOTAH-2
                     REB-S-TOTPP-1  REB-S-TOTPP-2  REB-S-TOTO1-1
                     REB-S-TOTO1-2  REB-S-TOTO2-1  REB-S-TOTO2-2
                     REB-S-TOTO3-1  REB-S-TOTO3-2  REB-S-TOTSC-1
                     REB-S-TOTSC-2  REB-S-TOTMF-1  REB-S-TOTMF-2
                     REB-S-TOTDF-1  REB-S-TOTDF-2  REB-S-TOTTOT-1
                     REB-S-TOTTOT-2.
      *----------------------------------------------------------------
       ZERO-S5-TOTALS.
           MOVE 0 TO APPL-S-TOT01 APPL-S-TOT02 APPL-S-TOT03 APPL-S-TOT04
                     APPL-S-TOT05 APPL-S-TOT06 APPL-S-TOT07 APPL-S-TOT08
                     APPL-S-TOT09 APPL-S-TOT10.
      *----------------------------------------------------------------
       ZERO-S6-TOTALS.
           MOVE 0 TO REPO-S-TOTOUTS        REPO-S-TOTPRIN-MTD
                     REPO-S-TOTPRIN-YTD    REPO-S-TOTPRIN-PCT
                     REPO-S-TOTVOL-MTD     REPO-S-TOTVOL-YTD
                     REPO-S-TOTVOL-LYR     REPO-S-TOTVOL-PCT
                     REPO-S-TOTACCTS       REPO-S-TOTACCTS-MTD
                     REPO-S-TOTACCTS-YTD   REPO-S-TOTNWACCTS-MTD
                     REPO-S-TOTNWACCTS-YTD REPO-S-TOTNWACCTS-LYR.
      *----------------------------------------------------------------
       ZERO-P1-TOTALS.
           MOVE 0 TO OUTS-P-TOTOUTS        OUTS-P-TOTPRIN-MTD
                     OUTS-P-TOTPRIN-YTD    OUTS-P-TOTPRIN-PCT
                     OUTS-P-TOTVOL-MTD     OUTS-P-TOTVOL-YTD
                     OUTS-P-TOTVOL-LYR     OUTS-P-TOTVOL-PCT
                     OUTS-P-TOTACCTS       OUTS-P-TOTACCTS-MTD
                     OUTS-P-TOTACCTS-YTD   OUTS-P-TOTNWACCTS-MTD
                     OUTS-P-TOTNWACCTS-YTD OUTS-P-TOTNWACCTS-LYR.
      *----------------------------------------------------------------
       ZERO-P2-TOTALS.
           MOVE 0 TO PL-P-TOTOUTS        PL-P-TOTPRIN-MTD
                     PL-P-TOTPRIN-YTD    PL-P-TOTPRIN-PCT
                     PL-P-TOTVOL-MTD     PL-P-TOTVOL-YTD
                     PL-P-TOTVOL-LYR     PL-P-TOTVOL-PCT
                     PL-P-TOTACCTS       PL-P-TOTACCTS-MTD
                     PL-P-TOTACCTS-YTD   PL-P-TOTNWACCTS-MTD
                     PL-P-TOTNWACCTS-YTD PL-P-TOTNWACCTS-LYR.
      *----------------------------------------------------------------
       ZERO-P3-TOTALS.
           MOVE 0 TO COL-P-TOTOTH-MTD  COL-P-TOTOTH-YTD
                     COL-P-TOTINT-MTD  COL-P-TOTINT-YTD
                     COL-P-TOTLATE-MTD COL-P-TOTLATE-YTD
                     COL-P-TOTPRIN-MTD COL-P-TOTPRIN-YTD
                     COL-P-TOTDEF-MTD  COL-P-TOTDEF-YTD
                     COL-P-TOTPLA-MTD  COL-P-TOTPLA-YTD
                     COL-P-TOTPLI-MTD  COL-P-TOTPLI-YTD
                     COL-P-TOTRNP-MTD  COL-P-TOTRNP-YTD
                     COL-P-TOTTOT-MTD  COL-P-TOTTOT-YTD.
      *----------------------------------------------------------------
       ZERO-P4-TOTALS.
           MOVE 0 TO REB-P-TOTINT-1 REB-P-TOTINT-2 REB-P-TOTCL-1
                     REB-P-TOTCL-2  REB-P-TOTAH-1  REB-P-TOTAH-2
                     REB-P-TOTPP-1  REB-P-TOTPP-2  REB-P-TOTO1-1
                     REB-P-TOTO1-2  REB-P-TOTO2-1  REB-P-TOTO2-2
                     REB-P-TOTO3-1  REB-P-TOTO3-2  REB-P-TOTSC-1
                     REB-P-TOTSC-2  REB-P-TOTMF-1  REB-P-TOTMF-2
                     REB-P-TOTDF-1  REB-P-TOTDF-2  REB-P-TOTTOT-1
                     REB-P-TOTTOT-2.
      *----------------------------------------------------------------
       ZERO-P5-TOTALS.
           MOVE 0 TO APPL-P-TOT01 APPL-P-TOT02 APPL-P-TOT03 APPL-P-TOT04
                     APPL-P-TOT05 APPL-P-TOT06 APPL-P-TOT07 APPL-P-TOT08
                     APPL-P-TOT09 APPL-P-TOT10.
      *----------------------------------------------------------------
       ZERO-P6-TOTALS.
           MOVE 0 TO REPO-P-TOTOUTS        REPO-P-TOTPRIN-MTD
                     REPO-P-TOTPRIN-YTD    REPO-P-TOTPRIN-PCT
                     REPO-P-TOTVOL-MTD     REPO-P-TOTVOL-YTD
                     REPO-P-TOTVOL-LYR     REPO-P-TOTVOL-PCT
                     REPO-P-TOTACCTS       REPO-P-TOTACCTS-MTD
                     REPO-P-TOTACCTS-YTD   REPO-P-TOTNWACCTS-MTD
                     REPO-P-TOTNWACCTS-YTD REPO-P-TOTNWACCTS-LYR.
      *----------------------------------------------------------------
       BEGIN-BY-FILE.
           MOVE SPACE TO E-O-FLAG.

           MOVE HOLD-BY-DATE-CCYYMM TO BY-DATE-CCYYMM
                                       QBY1-WBEG-DATE-CCYYMM
                                       QBY1-WEND-DATE-CCYYMM.
           MOVE BR-NO               TO BY-BRNO HOLD-BRANCH
                                       QBY1-WBEG-BRNO
                                       QBY1-WEND-BRNO.
           IF BEG-CLASS = 0
               MOVE 0         TO BY-CLASS
           ELSE
               MOVE BEG-CLASS TO BY-CLASS.
           MOVE BY-CLASS            TO QBY1-WBEG-CLASS.
           MOVE ALL "9"             TO QBY1-WEND-CLASS.
           

           PERFORM START-BY1-FILE.
      *----------------------------------------------------------------
       GET-BRANCH.
           MOVE BR-NO TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.
      *----------------------------------------------------------------
       GET-TODAYS-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           MOVE ALP-MO TO MONTH-MO DISPLAY-MO.
           MOVE ALP-YR TO MONTH-YR DISPLAY-YR.
           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************
       IO-ROUTINES SECTION.
      *****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".  
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".


      * COPY "LIBLP/LPBY1RN.CPY".
      * COPYMEMBER: LIBLP/LPBY1RN
      ******************************************************************
      *
      * DESCRIPTION: BRANCH STATISTICS FILE - IO ROUTINES - A THRU B
      *
      * FILE KEY   : #1 = DATE-CCYYMM, BRNO, CLASS
      *
      * FILE PATH  : /USR/DATA/R#/LP/BYAFILE
      *              /USR/DATA/R#/LP/BYBFILE
      *
      * ATTENTION  : BY-PATH-OVERRIDE CONTROLS ALL FILES - A THRU B
      *              ^^^^^^^^^^^^^^^^
      *              THERE IS NO BYA-PATH-OVERRIDE OR BYB-PATH-OVERRIDE
      *
      *=================================================================
      * REV:
      * KEC 2019-0205 ***NEW*** TO A32; MODIFIED FROM A30
      * KEC 2021-0524 ADDED ACUSQL VERSION
      ******************************************************************
      *-----------------------------------------------------------------
       SET-ALL-TO-BY1.
           MOVE BYA1-KEY   TO BY1-KEY.
           MOVE BYA-AREA-A TO BY-AREA-A.
           MOVE BYB-AREA-B TO BY-AREA-B.

      *-----------------------------------------------------------------
       SET-BY1-TO-ALL.
           MOVE BY1-KEY    TO BYA1-KEY BYB1-KEY.
           MOVE BY-AREA-A  TO BYA-AREA-A.
           MOVE BY-AREA-B  TO BYB-AREA-B.

      *-----------------------------------------------------------------
       LOAD-BY1-FILE.
           PERFORM LOAD-BYA1-FILE.

      *-----------------------------------------------------------------
       OPEN-BY1-FILE.
 
           PERFORM OPEN-BYA1-FILE.
           IF ( IO-FG  = 0 )
              MOVE " " TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       START-BY1-FILE.
 
           PERFORM SET-BY1-TO-ALL.
           PERFORM START-BYA1-FILE. *> SSP - BYA,BYB
           IF ( IO-FG  = 0 )
              MOVE " " TO IO-MULTI-FG 
           ELSE                     
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-BY1-FILE-NEXT.
     
           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE-NEXT. *> EMBEDDED - BYA,BYB
           IF ( IO-FG = 0 )
              PERFORM SET-ALL-TO-BY1
           ELSE
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       CLOSE-BY1-FILE.

           PERFORM CLOSE-BYA1-FILE.

      *=================================================================
       LOAD-BYA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BY-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BYA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BYA-ALL-BASE
              MOVE FILPATH-MACHINE         TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH.

      *-----------------------------------------------------------------
       OPEN-BYA1-FILE.
           PERFORM LOAD-BYA1-FILE.
           PERFORM OPEN-IT.
           MOVE BYA-PATH-SQL TO E-FILE.

           IF BY1-CONN-OPEN-SW = "N"
              MOVE "Y" TO BY1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BY1_CONNECT
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.
		   
      *-----------------------------------------------------------------
       START-BYA1-FILE. *> SSP - BYA,BYB,LC 
           PERFORM START-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.

           IF ( BYA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BYA1-FILE.

           IF ( QBY1-WEND-DATE-CCYYMM = ZEROES ) OR
              ( QBY1-WEND-DATE-CCYYMM NOT NUMERIC )
              MOVE BY-DATE-CCYYMM       TO QBY1-WBEG-DATE-CCYYMM
              MOVE ALL "9"              TO QBY1-WEND-DATE-CCYYMM 
              MOVE BY-BRNO              TO QBY1-WBEG-BRNO
              MOVE ALL "9"              TO QBY1-WEND-BRNO
              MOVE BY-CLASS             TO QBY1-WBEG-CLASS
              MOVE ALL "9"              TO QBY1-WEND-CLASS.


      * STORED PROCEDURE SELECT STATEMENT: 
      *
      *  SELECT BYAFILE.BYA_DATE_CCYYMM,
      *         BYAFILE.BYA_BRNO,
      *         BYAFILE.BYA_CLASS,
      *         BYAFILE.BYA_NO_1_1_1,
      *         BYAFILE.BYA_NO_1_1_2,
      *         BYAFILE.BYA_NO_1_1_3,
      *         BYAFILE.BYA_NO_1_1_4,
      *         BYAFILE.BYA_NO_1_1_5,
      *         BYAFILE.BYA_NO_1_1_6,
      *         BYAFILE.BYA_NO_1_1_7,
      *         BYAFILE.BYA_NO_1_1_8,
      *         BYAFILE.BYA_NO_1_1_9,
      *         BYAFILE.BYA_NO_1_1_10,
      *         BYAFILE.BYA_NO_1_1_11,
      *         BYAFILE.BYA_NO_1_2_1,
      *         BYAFILE.BYA_NO_1_2_2,
      *         BYAFILE.BYA_NO_1_2_3,
      *         BYAFILE.BYA_NO_1_2_4,
      *         BYAFILE.BYA_NO_1_2_5,
      *         BYAFILE.BYA_NO_1_2_6,
      *         BYAFILE.BYA_NO_1_2_7,
      *         BYAFILE.BYA_NO_1_2_8,
      *         BYAFILE.BYA_NO_1_2_9,
      *         BYAFILE.BYA_NO_1_2_10,
      *         BYAFILE.BYA_NO_1_2_11,
      *         BYAFILE.BYA_NO_2_1_1,
      *         BYAFILE.BYA_NO_2_1_2,
      *         BYAFILE.BYA_NO_2_1_3,
      *         BYAFILE.BYA_NO_2_1_4,
      *         BYAFILE.BYA_NO_2_1_5,
      *         BYAFILE.BYA_NO_2_1_6,
      *         BYAFILE.BYA_NO_2_1_7,
      *         BYAFILE.BYA_NO_2_1_8,
      *         BYAFILE.BYA_NO_2_1_9,
      *         BYAFILE.BYA_NO_2_1_10,
      *         BYAFILE.BYA_NO_2_1_11,
      *         BYAFILE.BYA_NO_2_2_1,
      *         BYAFILE.BYA_NO_2_2_2,
      *         BYAFILE.BYA_NO_2_2_3,
      *         BYAFILE.BYA_NO_2_2_4,
      *         BYAFILE.BYA_NO_2_2_5,
      *         BYAFILE.BYA_NO_2_2_6,
      *         BYAFILE.BYA_NO_2_2_7,
      *         BYAFILE.BYA_NO_2_2_8,
      *         BYAFILE.BYA_NO_2_2_9,
      *         BYAFILE.BYA_NO_2_2_10,
      *         BYAFILE.BYA_NO_2_2_11,
      *         BYAFILE.BYA_NO_3_1_1,
      *         BYAFILE.BYA_NO_3_1_2,
      *         BYAFILE.BYA_NO_3_1_3,
      *         BYAFILE.BYA_NO_3_1_4,
      *         BYAFILE.BYA_NO_3_1_5,
      *         BYAFILE.BYA_NO_3_1_6,
      *         BYAFILE.BYA_NO_3_1_7,
      *         BYAFILE.BYA_NO_3_1_8,
      *         BYAFILE.BYA_NO_3_1_9,
      *         BYAFILE.BYA_NO_3_1_10,
      *         BYAFILE.BYA_NO_3_1_11,
      *         BYAFILE.BYA_NO_3_2_1,
      *         BYAFILE.BYA_NO_3_2_2,
      *         BYAFILE.BYA_NO_3_2_3,
      *         BYAFILE.BYA_NO_3_2_4,
      *         BYAFILE.BYA_NO_3_2_5,
      *         BYAFILE.BYA_NO_3_2_6,
      *         BYAFILE.BYA_NO_3_2_7,
      *         BYAFILE.BYA_NO_3_2_8,
      *         BYAFILE.BYA_NO_3_2_9,
      *         BYAFILE.BYA_NO_3_2_10,
      *         BYAFILE.BYA_NO_3_2_11,
      *         BYAFILE.BYA_AMT_1_1_1,
      *         BYAFILE.BYA_AMT_1_1_2,
      *         BYAFILE.BYA_AMT_1_1_3,
      *         BYAFILE.BYA_AMT_1_1_4,
      *         BYAFILE.BYA_AMT_1_1_5,
      *         BYAFILE.BYA_AMT_1_1_6,
      *         BYAFILE.BYA_AMT_1_1_7,
      *         BYAFILE.BYA_AMT_1_1_8,
      *         BYAFILE.BYA_AMT_1_1_9,
      *         BYAFILE.BYA_AMT_1_1_10,
      *         BYAFILE.BYA_AMT_1_1_11,
      *         BYAFILE.BYA_AMT_1_2_1,
      *         BYAFILE.BYA_AMT_1_2_2,
      *         BYAFILE.BYA_AMT_1_2_3,
      *         BYAFILE.BYA_AMT_1_2_4,
      *         BYAFILE.BYA_AMT_1_2_5,
      *         BYAFILE.BYA_AMT_1_2_6,
      *         BYAFILE.BYA_AMT_1_2_7,
      *         BYAFILE.BYA_AMT_1_2_8,
      *         BYAFILE.BYA_AMT_1_2_9,
      *         BYAFILE.BYA_AMT_1_2_10,
      *         BYAFILE.BYA_AMT_1_2_11,
      *         BYAFILE.BYA_AMT_2_1_1,
      *         BYAFILE.BYA_AMT_2_1_2,
      *         BYAFILE.BYA_AMT_2_1_3,
      *         BYAFILE.BYA_AMT_2_1_4,
      *         BYAFILE.BYA_AMT_2_1_5,
      *         BYAFILE.BYA_AMT_2_1_6,
      *         BYAFILE.BYA_AMT_2_1_7,
      *         BYAFILE.BYA_AMT_2_1_8,
      *         BYAFILE.BYA_AMT_2_1_9,
      *         BYAFILE.BYA_AMT_2_1_10,
      *         BYAFILE.BYA_AMT_2_1_11,
      *         BYAFILE.BYA_AMT_2_2_1,
      *         BYAFILE.BYA_AMT_2_2_2,
      *         BYAFILE.BYA_AMT_2_2_3,
      *         BYAFILE.BYA_AMT_2_2_4,
      *         BYAFILE.BYA_AMT_2_2_5,
      *         BYAFILE.BYA_AMT_2_2_6,
      *         BYAFILE.BYA_AMT_2_2_7,
      *         BYAFILE.BYA_AMT_2_2_8,
      *         BYAFILE.BYA_AMT_2_2_9,
      *         BYAFILE.BYA_AMT_2_2_10,
      *         BYAFILE.BYA_AMT_2_2_11,
      *         BYAFILE.BYA_AMT_3_1_1,
      *         BYAFILE.BYA_AMT_3_1_2,
      *         BYAFILE.BYA_AMT_3_1_3,
      *         BYAFILE.BYA_AMT_3_1_4,
      *         BYAFILE.BYA_AMT_3_1_5,
      *         BYAFILE.BYA_AMT_3_1_6,
      *         BYAFILE.BYA_AMT_3_1_7,
      *         BYAFILE.BYA_AMT_3_1_8,
      *         BYAFILE.BYA_AMT_3_1_9,
      *         BYAFILE.BYA_AMT_3_1_10,
      *         BYAFILE.BYA_AMT_3_1_11,
      *         BYAFILE.BYA_AMT_3_2_1,
      *         BYAFILE.BYA_AMT_3_2_2,
      *         BYAFILE.BYA_AMT_3_2_3,
      *         BYAFILE.BYA_AMT_3_2_4,
      *         BYAFILE.BYA_AMT_3_2_5,
      *         BYAFILE.BYA_AMT_3_2_6,
      *         BYAFILE.BYA_AMT_3_2_7,
      *         BYAFILE.BYA_AMT_3_2_8,
      *         BYAFILE.BYA_AMT_3_2_9,
      *         BYAFILE.BYA_AMT_3_2_10,
      *         BYAFILE.BYA_AMT_3_2_11,
      *         BYAFILE.BYA_O2_1_1_1,
      *         BYAFILE.BYA_O2_1_1_2,
      *         BYAFILE.BYA_O2_1_1_3,
      *         BYAFILE.BYA_O2_1_1_4,
      *         BYAFILE.BYA_O2_1_1_5,
      *         BYAFILE.BYA_O2_1_1_6,
      *         BYAFILE.BYA_O2_1_1_7,
      *         BYAFILE.BYA_O2_1_1_8,
      *         BYAFILE.BYA_O2_1_1_9,
      *         BYAFILE.BYA_O2_1_1_10,
      *         BYAFILE.BYA_O2_1_1_11,
      *         BYAFILE.BYA_O2_1_2_1,
      *         BYAFILE.BYA_O2_1_2_2,
      *         BYAFILE.BYA_O2_1_2_3,
      *         BYAFILE.BYA_O2_1_2_4,
      *         BYAFILE.BYA_O2_1_2_5,
      *         BYAFILE.BYA_O2_1_2_6,
      *         BYAFILE.BYA_O2_1_2_7,
      *         BYAFILE.BYA_O2_1_2_8,
      *         BYAFILE.BYA_O2_1_2_9,
      *         BYAFILE.BYA_O2_1_2_10,
      *         BYAFILE.BYA_O2_1_2_11,
      *         BYAFILE.BYA_O2_2_1_1,
      *         BYAFILE.BYA_O2_2_1_2,
      *         BYAFILE.BYA_O2_2_1_3,
      *         BYAFILE.BYA_O2_2_1_4,
      *         BYAFILE.BYA_O2_2_1_5,
      *         BYAFILE.BYA_O2_2_1_6,
      *         BYAFILE.BYA_O2_2_1_7,
      *         BYAFILE.BYA_O2_2_1_8,
      *         BYAFILE.BYA_O2_2_1_9,
      *         BYAFILE.BYA_O2_2_1_10,
      *         BYAFILE.BYA_O2_2_1_11,
      *         BYAFILE.BYA_O2_2_2_1,
      *         BYAFILE.BYA_O2_2_2_2,
      *         BYAFILE.BYA_O2_2_2_3,
      *         BYAFILE.BYA_O2_2_2_4,
      *         BYAFILE.BYA_O2_2_2_5,
      *         BYAFILE.BYA_O2_2_2_6,
      *         BYAFILE.BYA_O2_2_2_7,
      *         BYAFILE.BYA_O2_2_2_8,
      *         BYAFILE.BYA_O2_2_2_9,
      *         BYAFILE.BYA_O2_2_2_10,
      *         BYAFILE.BYA_O2_2_2_11,
      *         BYAFILE.BYA_O2_3_1_1,
      *         BYAFILE.BYA_O2_3_1_2,
      *         BYAFILE.BYA_O2_3_1_3,
      *         BYAFILE.BYA_O2_3_1_4,
      *         BYAFILE.BYA_O2_3_1_5,
      *         BYAFILE.BYA_O2_3_1_6,
      *         BYAFILE.BYA_O2_3_1_7,
      *         BYAFILE.BYA_O2_3_1_8,
      *         BYAFILE.BYA_O2_3_1_9,
      *         BYAFILE.BYA_O2_3_1_10,
      *         BYAFILE.BYA_O2_3_1_11,
      *         BYAFILE.BYA_O2_3_2_1,
      *         BYAFILE.BYA_O2_3_2_2,
      *         BYAFILE.BYA_O2_3_2_3,
      *         BYAFILE.BYA_O2_3_2_4,
      *         BYAFILE.BYA_O2_3_2_5,
      *         BYAFILE.BYA_O2_3_2_6,
      *         BYAFILE.BYA_O2_3_2_7,
      *         BYAFILE.BYA_O2_3_2_8,
      *         BYAFILE.BYA_O2_3_2_9,
      *         BYAFILE.BYA_O2_3_2_10,
      *         BYAFILE.BYA_O2_3_2_11,
      *         BYAFILE.BYA_INT_1_1_1,
      *         BYAFILE.BYA_INT_1_1_2,
      *         BYAFILE.BYA_INT_1_1_3,
      *         BYAFILE.BYA_INT_1_1_4,
      *         BYAFILE.BYA_INT_1_1_5,
      *         BYAFILE.BYA_INT_1_1_6,
      *         BYAFILE.BYA_INT_1_1_7,
      *         BYAFILE.BYA_INT_1_1_8,
      *         BYAFILE.BYA_INT_1_1_9,
      *         BYAFILE.BYA_INT_1_1_10,
      *         BYAFILE.BYA_INT_1_1_11,
      *         BYAFILE.BYA_INT_1_2_1,
      *         BYAFILE.BYA_INT_1_2_2,
      *         BYAFILE.BYA_INT_1_2_3,
      *         BYAFILE.BYA_INT_1_2_4,
      *         BYAFILE.BYA_INT_1_2_5,
      *         BYAFILE.BYA_INT_1_2_6,
      *         BYAFILE.BYA_INT_1_2_7,
      *         BYAFILE.BYA_INT_1_2_8,
      *         BYAFILE.BYA_INT_1_2_9,
      *         BYAFILE.BYA_INT_1_2_10,
      *         BYAFILE.BYA_INT_1_2_11,
      *         BYAFILE.BYA_INT_2_1_1,
      *         BYAFILE.BYA_INT_2_1_2,
      *         BYAFILE.BYA_INT_2_1_3,
      *         BYAFILE.BYA_INT_2_1_4,
      *         BYAFILE.BYA_INT_2_1_5,
      *         BYAFILE.BYA_INT_2_1_6,
      *         BYAFILE.BYA_INT_2_1_7,
      *         BYAFILE.BYA_INT_2_1_8,
      *         BYAFILE.BYA_INT_2_1_9,
      *         BYAFILE.BYA_INT_2_1_10,
      *         BYAFILE.BYA_INT_2_1_11,
      *         BYAFILE.BYA_INT_2_2_1,
      *         BYAFILE.BYA_INT_2_2_2,
      *         BYAFILE.BYA_INT_2_2_3,
      *         BYAFILE.BYA_INT_2_2_4,
      *         BYAFILE.BYA_INT_2_2_5,
      *         BYAFILE.BYA_INT_2_2_6,
      *         BYAFILE.BYA_INT_2_2_7,
      *         BYAFILE.BYA_INT_2_2_8,
      *         BYAFILE.BYA_INT_2_2_9,
      *         BYAFILE.BYA_INT_2_2_10,
      *         BYAFILE.BYA_INT_2_2_11,
      *         BYAFILE.BYA_INT_3_1_1,
      *         BYAFILE.BYA_INT_3_1_2,
      *         BYAFILE.BYA_INT_3_1_3,
      *         BYAFILE.BYA_INT_3_1_4,
      *         BYAFILE.BYA_INT_3_1_5,
      *         BYAFILE.BYA_INT_3_1_6,
      *         BYAFILE.BYA_INT_3_1_7,
      *         BYAFILE.BYA_INT_3_1_8,
      *         BYAFILE.BYA_INT_3_1_9,
      *         BYAFILE.BYA_INT_3_1_10,
      *         BYAFILE.BYA_INT_3_1_11,
      *         BYAFILE.BYA_INT_3_2_1,
      *         BYAFILE.BYA_INT_3_2_2,
      *         BYAFILE.BYA_INT_3_2_3,
      *         BYAFILE.BYA_INT_3_2_4,
      *         BYAFILE.BYA_INT_3_2_5,
      *         BYAFILE.BYA_INT_3_2_6,
      *         BYAFILE.BYA_INT_3_2_7,
      *         BYAFILE.BYA_INT_3_2_8,
      *         BYAFILE.BYA_INT_3_2_9,
      *         BYAFILE.BYA_INT_3_2_10,
      *         BYAFILE.BYA_INT_3_2_11,
      *         BYAFILE.BYA_NO_REPO_1_1_1,
      *         BYAFILE.BYA_NO_REPO_1_1_2,
      *         BYAFILE.BYA_NO_REPO_1_1_3,
      *         BYAFILE.BYA_NO_REPO_1_1_4,
      *         BYAFILE.BYA_NO_REPO_1_1_5,
      *         BYAFILE.BYA_NO_REPO_1_2_1,
      *         BYAFILE.BYA_NO_REPO_1_2_2,
      *         BYAFILE.BYA_NO_REPO_1_2_3,
      *         BYAFILE.BYA_NO_REPO_1_2_4,
      *         BYAFILE.BYA_NO_REPO_1_2_5,
      *         BYAFILE.BYA_NO_REPO_2_1_1,
      *         BYAFILE.BYA_NO_REPO_2_1_2,
      *         BYAFILE.BYA_NO_REPO_2_1_3,
      *         BYAFILE.BYA_NO_REPO_2_1_4,
      *         BYAFILE.BYA_NO_REPO_2_1_5,
      *         BYAFILE.BYA_NO_REPO_2_2_1,
      *         BYAFILE.BYA_NO_REPO_2_2_2,
      *         BYAFILE.BYA_NO_REPO_2_2_3,
      *         BYAFILE.BYA_NO_REPO_2_2_4,
      *         BYAFILE.BYA_NO_REPO_2_2_5,
      *         BYAFILE.BYA_NO_REPO_3_1_1,
      *         BYAFILE.BYA_NO_REPO_3_1_2,
      *         BYAFILE.BYA_NO_REPO_3_1_3,
      *         BYAFILE.BYA_NO_REPO_3_1_4,
      *         BYAFILE.BYA_NO_REPO_3_1_5,
      *         BYAFILE.BYA_NO_REPO_3_2_1,
      *         BYAFILE.BYA_NO_REPO_3_2_2,
      *         BYAFILE.BYA_NO_REPO_3_2_3,
      *         BYAFILE.BYA_NO_REPO_3_2_4,
      *         BYAFILE.BYA_NO_REPO_3_2_5,
      *         BYAFILE.BYA_AMT_REPO_1_1_1,
      *         BYAFILE.BYA_AMT_REPO_1_1_2,
      *         BYAFILE.BYA_AMT_REPO_1_1_3,
      *         BYAFILE.BYA_AMT_REPO_1_1_4,
      *         BYAFILE.BYA_AMT_REPO_1_1_5,
      *         BYAFILE.BYA_AMT_REPO_1_2_1,
      *         BYAFILE.BYA_AMT_REPO_1_2_2,
      *         BYAFILE.BYA_AMT_REPO_1_2_3,
      *         BYAFILE.BYA_AMT_REPO_1_2_4,
      *         BYAFILE.BYA_AMT_REPO_1_2_5,
      *         BYAFILE.BYA_AMT_REPO_2_1_1,
      *         BYAFILE.BYA_AMT_REPO_2_1_2,
      *         BYAFILE.BYA_AMT_REPO_2_1_3,
      *         BYAFILE.BYA_AMT_REPO_2_1_4,
      *         BYAFILE.BYA_AMT_REPO_2_1_5,
      *         BYAFILE.BYA_AMT_REPO_2_2_1,
      *         BYAFILE.BYA_AMT_REPO_2_2_2,
      *         BYAFILE.BYA_AMT_REPO_2_2_3,
      *         BYAFILE.BYA_AMT_REPO_2_2_4,
      *         BYAFILE.BYA_AMT_REPO_2_2_5,
      *         BYAFILE.BYA_AMT_REPO_3_1_1,
      *         BYAFILE.BYA_AMT_REPO_3_1_2,
      *         BYAFILE.BYA_AMT_REPO_3_1_3,
      *         BYAFILE.BYA_AMT_REPO_3_1_4,
      *         BYAFILE.BYA_AMT_REPO_3_1_5,
      *         BYAFILE.BYA_AMT_REPO_3_2_1,
      *         BYAFILE.BYA_AMT_REPO_3_2_2,
      *         BYAFILE.BYA_AMT_REPO_3_2_3,
      *         BYAFILE.BYA_AMT_REPO_3_2_4,
      *         BYAFILE.BYA_AMT_REPO_3_2_5,
      *         BYAFILE.BYA_O2_REPO_1_1_1,
      *         BYAFILE.BYA_O2_REPO_1_1_2,
      *         BYAFILE.BYA_O2_REPO_1_1_3,
      *         BYAFILE.BYA_O2_REPO_1_1_4,
      *         BYAFILE.BYA_O2_REPO_1_1_5,
      *         BYAFILE.BYA_O2_REPO_1_2_1,
      *         BYAFILE.BYA_O2_REPO_1_2_2,
      *         BYAFILE.BYA_O2_REPO_1_2_3,
      *         BYAFILE.BYA_O2_REPO_1_2_4,
      *         BYAFILE.BYA_O2_REPO_1_2_5,
      *         BYAFILE.BYA_O2_REPO_2_1_1,
      *         BYAFILE.BYA_O2_REPO_2_1_2,
      *         BYAFILE.BYA_O2_REPO_2_1_3,
      *         BYAFILE.BYA_O2_REPO_2_1_4,
      *         BYAFILE.BYA_O2_REPO_2_1_5,
      *         BYAFILE.BYA_O2_REPO_2_2_1,
      *         BYAFILE.BYA_O2_REPO_2_2_2,
      *         BYAFILE.BYA_O2_REPO_2_2_3,
      *         BYAFILE.BYA_O2_REPO_2_2_4,
      *         BYAFILE.BYA_O2_REPO_2_2_5,
      *         BYAFILE.BYA_O2_REPO_3_1_1,
      *         BYAFILE.BYA_O2_REPO_3_1_2,
      *         BYAFILE.BYA_O2_REPO_3_1_3,
      *         BYAFILE.BYA_O2_REPO_3_1_4,
      *         BYAFILE.BYA_O2_REPO_3_1_5,
      *         BYAFILE.BYA_O2_REPO_3_2_1,
      *         BYAFILE.BYA_O2_REPO_3_2_2,
      *         BYAFILE.BYA_O2_REPO_3_2_3,
      *         BYAFILE.BYA_O2_REPO_3_2_4,
      *         BYAFILE.BYA_O2_REPO_3_2_5,
      *         BYAFILE.BYA_INT_REPO_1_1_1,
      *         BYAFILE.BYA_INT_REPO_1_1_2,
      *         BYAFILE.BYA_INT_REPO_1_1_3,
      *         BYAFILE.BYA_INT_REPO_1_1_4,
      *         BYAFILE.BYA_INT_REPO_1_1_5,
      *         BYAFILE.BYA_INT_REPO_1_2_1,
      *         BYAFILE.BYA_INT_REPO_1_2_2,
      *         BYAFILE.BYA_INT_REPO_1_2_3,
      *         BYAFILE.BYA_INT_REPO_1_2_4,
      *         BYAFILE.BYA_INT_REPO_1_2_5,
      *         BYAFILE.BYA_INT_REPO_2_1_1,
      *         BYAFILE.BYA_INT_REPO_2_1_2,
      *         BYAFILE.BYA_INT_REPO_2_1_3,
      *         BYAFILE.BYA_INT_REPO_2_1_4,
      *         BYAFILE.BYA_INT_REPO_2_1_5,
      *         BYAFILE.BYA_INT_REPO_2_2_1,
      *         BYAFILE.BYA_INT_REPO_2_2_2,
      *         BYAFILE.BYA_INT_REPO_2_2_3,
      *         BYAFILE.BYA_INT_REPO_2_2_4,
      *         BYAFILE.BYA_INT_REPO_2_2_5,
      *         BYAFILE.BYA_INT_REPO_3_1_1,
      *         BYAFILE.BYA_INT_REPO_3_1_2,
      *         BYAFILE.BYA_INT_REPO_3_1_3,
      *         BYAFILE.BYA_INT_REPO_3_1_4,
      *         BYAFILE.BYA_INT_REPO_3_1_5,
      *         BYAFILE.BYA_INT_REPO_3_2_1,
      *         BYAFILE.BYA_INT_REPO_3_2_2,
      *         BYAFILE.BYA_INT_REPO_3_2_3,
      *         BYAFILE.BYA_INT_REPO_3_2_4,
      *         BYAFILE.BYA_INT_REPO_3_2_5,
      *         BYAFILE.BYA_NO_PL_1_1_1,
      *         BYAFILE.BYA_NO_PL_1_1_2,
      *         BYAFILE.BYA_NO_PL_1_1_3,
      *         BYAFILE.BYA_NO_PL_1_1_4,
      *         BYAFILE.BYA_NO_PL_1_1_5,
      *         BYAFILE.BYA_NO_PL_1_1_6,
      *         BYAFILE.BYA_NO_PL_1_1_7,
      *         BYAFILE.BYA_NO_PL_1_1_8,
      *         BYAFILE.BYA_NO_PL_1_1_9,
      *         BYAFILE.BYA_NO_PL_1_1_10,
      *         BYAFILE.BYA_NO_PL_1_2_1,
      *         BYAFILE.BYA_NO_PL_1_2_2,
      *         BYAFILE.BYA_NO_PL_1_2_3,
      *         BYAFILE.BYA_NO_PL_1_2_4,
      *         BYAFILE.BYA_NO_PL_1_2_5,
      *         BYAFILE.BYA_NO_PL_1_2_6,
      *         BYAFILE.BYA_NO_PL_1_2_7,
      *         BYAFILE.BYA_NO_PL_1_2_8,
      *         BYAFILE.BYA_NO_PL_1_2_9,
      *         BYAFILE.BYA_NO_PL_1_2_10,
      *         BYAFILE.BYA_NO_PL_2_1_1,
      *         BYAFILE.BYA_NO_PL_2_1_2,
      *         BYAFILE.BYA_NO_PL_2_1_3,
      *         BYAFILE.BYA_NO_PL_2_1_4,
      *         BYAFILE.BYA_NO_PL_2_1_5,
      *         BYAFILE.BYA_NO_PL_2_1_6,
      *         BYAFILE.BYA_NO_PL_2_1_7,
      *         BYAFILE.BYA_NO_PL_2_1_8,
      *         BYAFILE.BYA_NO_PL_2_1_9,
      *         BYAFILE.BYA_NO_PL_2_1_10,
      *         BYAFILE.BYA_NO_PL_2_2_1,
      *         BYAFILE.BYA_NO_PL_2_2_2,
      *         BYAFILE.BYA_NO_PL_2_2_3,
      *         BYAFILE.BYA_NO_PL_2_2_4,
      *         BYAFILE.BYA_NO_PL_2_2_5,
      *         BYAFILE.BYA_NO_PL_2_2_6,
      *         BYAFILE.BYA_NO_PL_2_2_7,
      *         BYAFILE.BYA_NO_PL_2_2_8,
      *         BYAFILE.BYA_NO_PL_2_2_9,
      *         BYAFILE.BYA_NO_PL_2_2_10,
      *         BYAFILE.BYA_NO_PL_3_1_1,
      *         BYAFILE.BYA_NO_PL_3_1_2,
      *         BYAFILE.BYA_NO_PL_3_1_3,
      *         BYAFILE.BYA_NO_PL_3_1_4,
      *         BYAFILE.BYA_NO_PL_3_1_5,
      *         BYAFILE.BYA_NO_PL_3_1_6,
      *         BYAFILE.BYA_NO_PL_3_1_7,
      *         BYAFILE.BYA_NO_PL_3_1_8,
      *         BYAFILE.BYA_NO_PL_3_1_9,
      *         BYAFILE.BYA_NO_PL_3_1_10,
      *         BYAFILE.BYA_NO_PL_3_2_1,
      *         BYAFILE.BYA_NO_PL_3_2_2,
      *         BYAFILE.BYA_NO_PL_3_2_3,
      *         BYAFILE.BYA_NO_PL_3_2_4,
      *         BYAFILE.BYA_NO_PL_3_2_5,
      *         BYAFILE.BYA_NO_PL_3_2_6,
      *         BYAFILE.BYA_NO_PL_3_2_7,
      *         BYAFILE.BYA_NO_PL_3_2_8,
      *         BYAFILE.BYA_NO_PL_3_2_9,
      *         BYAFILE.BYA_NO_PL_3_2_10,
      *         BYAFILE.BYA_AMT_PL_1_1_1,
      *         BYAFILE.BYA_AMT_PL_1_1_2,
      *         BYAFILE.BYA_AMT_PL_1_1_3,
      *         BYAFILE.BYA_AMT_PL_1_1_4,
      *         BYAFILE.BYA_AMT_PL_1_1_5,
      *         BYAFILE.BYA_AMT_PL_1_1_6,
      *         BYAFILE.BYA_AMT_PL_1_1_7,
      *         BYAFILE.BYA_AMT_PL_1_1_8,
      *         BYAFILE.BYA_AMT_PL_1_1_9,
      *         BYAFILE.BYA_AMT_PL_1_1_10,
      *         BYAFILE.BYA_AMT_PL_1_2_1,
      *         BYAFILE.BYA_AMT_PL_1_2_2,
      *         BYAFILE.BYA_AMT_PL_1_2_3,
      *         BYAFILE.BYA_AMT_PL_1_2_4,
      *         BYAFILE.BYA_AMT_PL_1_2_5,
      *         BYAFILE.BYA_AMT_PL_1_2_6,
      *         BYAFILE.BYA_AMT_PL_1_2_7,
      *         BYAFILE.BYA_AMT_PL_1_2_8,
      *         BYAFILE.BYA_AMT_PL_1_2_9,
      *         BYAFILE.BYA_AMT_PL_1_2_10,
      *         BYAFILE.BYA_AMT_PL_2_1_1,
      *         BYAFILE.BYA_AMT_PL_2_1_2,
      *         BYAFILE.BYA_AMT_PL_2_1_3,
      *         BYAFILE.BYA_AMT_PL_2_1_4,
      *         BYAFILE.BYA_AMT_PL_2_1_5,
      *         BYAFILE.BYA_AMT_PL_2_1_6,
      *         BYAFILE.BYA_AMT_PL_2_1_7,
      *         BYAFILE.BYA_AMT_PL_2_1_8,
      *         BYAFILE.BYA_AMT_PL_2_1_9,
      *         BYAFILE.BYA_AMT_PL_2_1_10,
      *         BYAFILE.BYA_AMT_PL_2_2_1,
      *         BYAFILE.BYA_AMT_PL_2_2_2,
      *         BYAFILE.BYA_AMT_PL_2_2_3,
      *         BYAFILE.BYA_AMT_PL_2_2_4,
      *         BYAFILE.BYA_AMT_PL_2_2_5,
      *         BYAFILE.BYA_AMT_PL_2_2_6,
      *         BYAFILE.BYA_AMT_PL_2_2_7,
      *         BYAFILE.BYA_AMT_PL_2_2_8,
      *         BYAFILE.BYA_AMT_PL_2_2_9,
      *         BYAFILE.BYA_AMT_PL_2_2_10,
      *         BYAFILE.BYA_AMT_PL_3_1_1,
      *         BYAFILE.BYA_AMT_PL_3_1_2,
      *         BYAFILE.BYA_AMT_PL_3_1_3,
      *         BYAFILE.BYA_AMT_PL_3_1_4,
      *         BYAFILE.BYA_AMT_PL_3_1_5,
      *         BYAFILE.BYA_AMT_PL_3_1_6,
      *         BYAFILE.BYA_AMT_PL_3_1_7,
      *         BYAFILE.BYA_AMT_PL_3_1_8,
      *         BYAFILE.BYA_AMT_PL_3_1_9,
      *         BYAFILE.BYA_AMT_PL_3_1_10,
      *         BYAFILE.BYA_AMT_PL_3_2_1,
      *         BYAFILE.BYA_AMT_PL_3_2_2,
      *         BYAFILE.BYA_AMT_PL_3_2_3,
      *         BYAFILE.BYA_AMT_PL_3_2_4,
      *         BYAFILE.BYA_AMT_PL_3_2_5,
      *         BYAFILE.BYA_AMT_PL_3_2_6,
      *         BYAFILE.BYA_AMT_PL_3_2_7,
      *         BYAFILE.BYA_AMT_PL_3_2_8,
      *         BYAFILE.BYA_AMT_PL_3_2_9,
      *         BYAFILE.BYA_AMT_PL_3_2_10,
      *         BYAFILE.BYA_O2_PL_1_1_1,
      *         BYAFILE.BYA_O2_PL_1_1_2,
      *         BYAFILE.BYA_O2_PL_1_1_3,
      *         BYAFILE.BYA_O2_PL_1_1_4,
      *         BYAFILE.BYA_O2_PL_1_1_5,
      *         BYAFILE.BYA_O2_PL_1_1_6,
      *         BYAFILE.BYA_O2_PL_1_1_7,
      *         BYAFILE.BYA_O2_PL_1_1_8,
      *         BYAFILE.BYA_O2_PL_1_1_9,
      *         BYAFILE.BYA_O2_PL_1_1_10,
      *         BYAFILE.BYA_O2_PL_1_2_1,
      *         BYAFILE.BYA_O2_PL_1_2_2,
      *         BYAFILE.BYA_O2_PL_1_2_3,
      *         BYAFILE.BYA_O2_PL_1_2_4,
      *         BYAFILE.BYA_O2_PL_1_2_5,
      *         BYAFILE.BYA_O2_PL_1_2_6,
      *         BYAFILE.BYA_O2_PL_1_2_7,
      *         BYAFILE.BYA_O2_PL_1_2_8,
      *         BYAFILE.BYA_O2_PL_1_2_9,
      *         BYAFILE.BYA_O2_PL_1_2_10,
      *         BYAFILE.BYA_O2_PL_2_1_1,
      *         BYAFILE.BYA_O2_PL_2_1_2,
      *         BYAFILE.BYA_O2_PL_2_1_3,
      *         BYAFILE.BYA_O2_PL_2_1_4,
      *         BYAFILE.BYA_O2_PL_2_1_5,
      *         BYAFILE.BYA_O2_PL_2_1_6,
      *         BYAFILE.BYA_O2_PL_2_1_7,
      *         BYAFILE.BYA_O2_PL_2_1_8,
      *         BYAFILE.BYA_O2_PL_2_1_9,
      *         BYAFILE.BYA_O2_PL_2_1_10,
      *         BYAFILE.BYA_O2_PL_2_2_1,
      *         BYAFILE.BYA_O2_PL_2_2_2,
      *         BYAFILE.BYA_O2_PL_2_2_3,
      *         BYAFILE.BYA_O2_PL_2_2_4,
      *         BYAFILE.BYA_O2_PL_2_2_5,
      *         BYAFILE.BYA_O2_PL_2_2_6,
      *         BYAFILE.BYA_O2_PL_2_2_7,
      *         BYAFILE.BYA_O2_PL_2_2_8,
      *         BYAFILE.BYA_O2_PL_2_2_9,
      *         BYAFILE.BYA_O2_PL_2_2_10,
      *         BYAFILE.BYA_O2_PL_3_1_1,
      *         BYAFILE.BYA_O2_PL_3_1_2,
      *         BYAFILE.BYA_O2_PL_3_1_3,
      *         BYAFILE.BYA_O2_PL_3_1_4,
      *         BYAFILE.BYA_O2_PL_3_1_5,
      *         BYAFILE.BYA_O2_PL_3_1_6,
      *         BYAFILE.BYA_O2_PL_3_1_7,
      *         BYAFILE.BYA_O2_PL_3_1_8,
      *         BYAFILE.BYA_O2_PL_3_1_9,
      *         BYAFILE.BYA_O2_PL_3_1_10,
      *         BYAFILE.BYA_O2_PL_3_2_1,
      *         BYAFILE.BYA_O2_PL_3_2_2,
      *         BYAFILE.BYA_O2_PL_3_2_3,
      *         BYAFILE.BYA_O2_PL_3_2_4,
      *         BYAFILE.BYA_O2_PL_3_2_5,
      *         BYAFILE.BYA_O2_PL_3_2_6,
      *         BYAFILE.BYA_O2_PL_3_2_7,
      *         BYAFILE.BYA_O2_PL_3_2_8,
      *         BYAFILE.BYA_O2_PL_3_2_9,
      *         BYAFILE.BYA_O2_PL_3_2_10,
      *         BYAFILE.BYA_INT_PL_1_1_1,
      *         BYAFILE.BYA_INT_PL_1_1_2,
      *         BYAFILE.BYA_INT_PL_1_1_3,
      *         BYAFILE.BYA_INT_PL_1_1_4,
      *         BYAFILE.BYA_INT_PL_1_1_5,
      *         BYAFILE.BYA_INT_PL_1_1_6,
      *         BYAFILE.BYA_INT_PL_1_1_7,
      *         BYAFILE.BYA_INT_PL_1_1_8,
      *         BYAFILE.BYA_INT_PL_1_1_9,
      *         BYAFILE.BYA_INT_PL_1_1_10,
      *         BYAFILE.BYA_INT_PL_1_2_1,
      *         BYAFILE.BYA_INT_PL_1_2_2,
      *         BYAFILE.BYA_INT_PL_1_2_3,
      *         BYAFILE.BYA_INT_PL_1_2_4,
      *         BYAFILE.BYA_INT_PL_1_2_5,
      *         BYAFILE.BYA_INT_PL_1_2_6,
      *         BYAFILE.BYA_INT_PL_1_2_7,
      *         BYAFILE.BYA_INT_PL_1_2_8,
      *         BYAFILE.BYA_INT_PL_1_2_9,
      *         BYAFILE.BYA_INT_PL_1_2_10,
      *         BYAFILE.BYA_INT_PL_2_1_1,
      *         BYAFILE.BYA_INT_PL_2_1_2,
      *         BYAFILE.BYA_INT_PL_2_1_3,
      *         BYAFILE.BYA_INT_PL_2_1_4,
      *         BYAFILE.BYA_INT_PL_2_1_5,
      *         BYAFILE.BYA_INT_PL_2_1_6,
      *         BYAFILE.BYA_INT_PL_2_1_7,
      *         BYAFILE.BYA_INT_PL_2_1_8,
      *         BYAFILE.BYA_INT_PL_2_1_9,
      *         BYAFILE.BYA_INT_PL_2_1_10,
      *         BYAFILE.BYA_INT_PL_2_2_1,
      *         BYAFILE.BYA_INT_PL_2_2_2,
      *         BYAFILE.BYA_INT_PL_2_2_3,
      *         BYAFILE.BYA_INT_PL_2_2_4,
      *         BYAFILE.BYA_INT_PL_2_2_5,
      *         BYAFILE.BYA_INT_PL_2_2_6,
      *         BYAFILE.BYA_INT_PL_2_2_7,
      *         BYAFILE.BYA_INT_PL_2_2_8,
      *         BYAFILE.BYA_INT_PL_2_2_9,
      *         BYAFILE.BYA_INT_PL_2_2_10,
      *         BYAFILE.BYA_INT_PL_3_1_1,
      *         BYAFILE.BYA_INT_PL_3_1_2,
      *         BYAFILE.BYA_INT_PL_3_1_3,
      *         BYAFILE.BYA_INT_PL_3_1_4,
      *         BYAFILE.BYA_INT_PL_3_1_5,
      *         BYAFILE.BYA_INT_PL_3_1_6,
      *         BYAFILE.BYA_INT_PL_3_1_7,
      *         BYAFILE.BYA_INT_PL_3_1_8,
      *         BYAFILE.BYA_INT_PL_3_1_9,
      *         BYAFILE.BYA_INT_PL_3_1_10,
      *         BYAFILE.BYA_INT_PL_3_2_1,
      *         BYAFILE.BYA_INT_PL_3_2_2,
      *         BYAFILE.BYA_INT_PL_3_2_3,
      *         BYAFILE.BYA_INT_PL_3_2_4,
      *         BYAFILE.BYA_INT_PL_3_2_5,
      *         BYAFILE.BYA_INT_PL_3_2_6,
      *         BYAFILE.BYA_INT_PL_3_2_7,
      *         BYAFILE.BYA_INT_PL_3_2_8,
      *         BYAFILE.BYA_INT_PL_3_2_9,
      *         BYAFILE.BYA_INT_PL_3_2_10,
      *         BYAFILE.BYA_DLRVPRIN_1_1_1,
      *         BYAFILE.BYA_DLRVPRIN_1_1_2,
      *         BYAFILE.BYA_DLRVPRIN_1_1_3,
      *         BYAFILE.BYA_DLRVPRIN_1_2_1,
      *         BYAFILE.BYA_DLRVPRIN_1_2_2,
      *         BYAFILE.BYA_DLRVPRIN_1_2_3,
      *         BYAFILE.BYA_DLRVPRIN_2_1_1,
      *         BYAFILE.BYA_DLRVPRIN_2_1_2,
      *         BYAFILE.BYA_DLRVPRIN_2_1_3,
      *         BYAFILE.BYA_DLRVPRIN_2_2_1,
      *         BYAFILE.BYA_DLRVPRIN_2_2_2,
      *         BYAFILE.BYA_DLRVPRIN_2_2_3,
      *         BYAFILE.BYA_DLRVPRIN_3_1_1,
      *         BYAFILE.BYA_DLRVPRIN_3_1_2,
      *         BYAFILE.BYA_DLRVPRIN_3_1_3,
      *         BYAFILE.BYA_DLRVPRIN_3_2_1,
      *         BYAFILE.BYA_DLRVPRIN_3_2_2,
      *         BYAFILE.BYA_DLRVPRIN_3_2_3,
      *         BYAFILE.BYA_DLRVCHGS_1_1_1,
      *         BYAFILE.BYA_DLRVCHGS_1_1_2,
      *         BYAFILE.BYA_DLRVCHGS_1_1_3,
      *         BYAFILE.BYA_DLRVCHGS_1_2_1,
      *         BYAFILE.BYA_DLRVCHGS_1_2_2,
      *         BYAFILE.BYA_DLRVCHGS_1_2_3,
      *         BYAFILE.BYA_DLRVCHGS_2_1_1,
      *         BYAFILE.BYA_DLRVCHGS_2_1_2,
      *         BYAFILE.BYA_DLRVCHGS_2_1_3,
      *         BYAFILE.BYA_DLRVCHGS_2_2_1,
      *         BYAFILE.BYA_DLRVCHGS_2_2_2,
      *         BYAFILE.BYA_DLRVCHGS_2_2_3,
      *         BYAFILE.BYA_DLRVCHGS_3_1_1,
      *         BYAFILE.BYA_DLRVCHGS_3_1_2,
      *         BYAFILE.BYA_DLRVCHGS_3_1_3,
      *         BYAFILE.BYA_DLRVCHGS_3_2_1,
      *         BYAFILE.BYA_DLRVCHGS_3_2_2,
      *         BYAFILE.BYA_DLRVCHGS_3_2_3,
      *         BYAFILE.BYA_DLPARVHELD_1_1,
      *         BYAFILE.BYA_DLPARVHELD_1_2,
      *         BYAFILE.BYA_DLPARVHELD_2_1,
      *         BYAFILE.BYA_DLPARVHELD_2_2,
      *         BYAFILE.BYA_DLPARVHELD_3_1,
      *         BYAFILE.BYA_DLPARVHELD_3_2,
      *         BYBFILE.BYB_PL_PRINCOL_1_1,
      *         BYBFILE.BYB_PL_PRINCOL_1_2,
      *         BYBFILE.BYB_PL_PRINCOL_1_3,
      *         BYBFILE.BYB_PL_PRINCOL_1_4,
      *         BYBFILE.BYB_PL_PRINCOL_1_5,
      *         BYBFILE.BYB_PL_PRINCOL_2_1,
      *         BYBFILE.BYB_PL_PRINCOL_2_2,
      *         BYBFILE.BYB_PL_PRINCOL_2_3,
      *         BYBFILE.BYB_PL_PRINCOL_2_4,
      *         BYBFILE.BYB_PL_PRINCOL_2_5,
      *         BYBFILE.BYB_PL_INTCOL_1_1,
      *         BYBFILE.BYB_PL_INTCOL_1_2,
      *         BYBFILE.BYB_PL_INTCOL_1_3,
      *         BYBFILE.BYB_PL_INTCOL_1_4,
      *         BYBFILE.BYB_PL_INTCOL_1_5,
      *         BYBFILE.BYB_PL_INTCOL_2_1,
      *         BYBFILE.BYB_PL_INTCOL_2_2,
      *         BYBFILE.BYB_PL_INTCOL_2_3,
      *         BYBFILE.BYB_PL_INTCOL_2_4,
      *         BYBFILE.BYB_PL_INTCOL_2_5,
      *         BYBFILE.BYB_PL_LATECOL_1_1,
      *         BYBFILE.BYB_PL_LATECOL_1_2,
      *         BYBFILE.BYB_PL_LATECOL_1_3,
      *         BYBFILE.BYB_PL_LATECOL_1_4,
      *         BYBFILE.BYB_PL_LATECOL_1_5,
      *         BYBFILE.BYB_PL_LATECOL_2_1,
      *         BYBFILE.BYB_PL_LATECOL_2_2,
      *         BYBFILE.BYB_PL_LATECOL_2_3,
      *         BYBFILE.BYB_PL_LATECOL_2_4,
      *         BYBFILE.BYB_PL_LATECOL_2_5,
      *         BYBFILE.BYB_PL_O2COL_1_1,
      *         BYBFILE.BYB_PL_O2COL_1_2,
      *         BYBFILE.BYB_PL_O2COL_1_3,
      *         BYBFILE.BYB_PL_O2COL_1_4,
      *         BYBFILE.BYB_PL_O2COL_1_5,
      *         BYBFILE.BYB_PL_O2COL_2_1,
      *         BYBFILE.BYB_PL_O2COL_2_2,
      *         BYBFILE.BYB_PL_O2COL_2_3,
      *         BYBFILE.BYB_PL_O2COL_2_4,
      *         BYBFILE.BYB_PL_O2COL_2_5,
      *         BYBFILE.BYB_AMT_SOLD_1,
      *         BYBFILE.BYB_AMT_SOLD_2,
      *         BYBFILE.BYB_AMT_TRANS_1,
      *         BYBFILE.BYB_AMT_TRANS_2,
      *         BYBFILE.BYB_AMT_PURCH_1,
      *         BYBFILE.BYB_AMT_PURCH_2,
      *         BYBFILE.BYB_NO_SOLD_1,
      *         BYBFILE.BYB_NO_SOLD_2,
      *         BYBFILE.BYB_NO_TRANS_1,
      *         BYBFILE.BYB_NO_TRANS_2,
      *         BYBFILE.BYB_NO_PURCH_1,
      *         BYBFILE.BYB_NO_PURCH_2,
      *         BYBFILE.BYB_OTHCOL_1,
      *         BYBFILE.BYB_OTHCOL_2,
      *         BYBFILE.BYB_OT2COL_1,
      *         BYBFILE.BYB_OT2COL_2,
      *         BYBFILE.BYB_INTCOL_1,
      *         BYBFILE.BYB_INTCOL_2,
      *         BYBFILE.BYB_LATECOL_1,
      *         BYBFILE.BYB_LATECOL_2,
      *         BYBFILE.BYB_PRINCOL_1,
      *         BYBFILE.BYB_PRINCOL_2,
      *         BYBFILE.BYB_DEFCOL_1,
      *         BYBFILE.BYB_DEFCOL_2,
      *         BYBFILE.BYB_RNPRIN_1,
      *         BYBFILE.BYB_RNPRIN_2,
      *         BYBFILE.BYB_INTREB_1,
      *         BYBFILE.BYB_INTREB_2,
      *         BYBFILE.BYB_CLREB1_1,
      *         BYBFILE.BYB_CLREB1_2,
      *         BYBFILE.BYB_CLREB2_1,
      *         BYBFILE.BYB_CLREB2_2,
      *         BYBFILE.BYB_CLREB3_1,
      *         BYBFILE.BYB_CLREB3_2,
      *         BYBFILE.BYB_CLREB4_1,
      *         BYBFILE.BYB_CLREB4_2,
      *         BYBFILE.BYB_AHREB_1,
      *         BYBFILE.BYB_AHREB_2,
      *         BYBFILE.BYB_PPREB_1,
      *         BYBFILE.BYB_PPREB_2,
      *         BYBFILE.BYB_O1REB_1,
      *         BYBFILE.BYB_O1REB_2,
      *         BYBFILE.BYB_O2REB_1,
      *         BYBFILE.BYB_O2REB_2,
      *         BYBFILE.BYB_O3REB_1,
      *         BYBFILE.BYB_O3REB_2,
      *         BYBFILE.BYB_O4REB_1,
      *         BYBFILE.BYB_O4REB_2,
      *         BYBFILE.BYB_O5REB_1,
      *         BYBFILE.BYB_O5REB_2,
      *         BYBFILE.BYB_O6REB_1,
      *         BYBFILE.BYB_O6REB_2,
      *         BYBFILE.BYB_O7REB_1,
      *         BYBFILE.BYB_O7REB_2,
      *         BYBFILE.BYB_SCREB_1,
      *         BYBFILE.BYB_SCREB_2,
      *         BYBFILE.BYB_MFREB_1,
      *         BYBFILE.BYB_MFREB_2,
      *         BYBFILE.BYB_DFREB_1,
      *         BYBFILE.BYB_DFREB_2,
      *         BYBFILE.BYB_UN7_REB_1,
      *         BYBFILE.BYB_UN7_REB_2,
      *         BYBFILE.BYB_UN8_REB_1,
      *         BYBFILE.BYB_UN8_REB_2,
      *         BYBFILE.BYB_UN9_REB_1,
      *         BYBFILE.BYB_UN9_REB_2,
      *         BYBFILE.BYB_UN10_REB_1,
      *         BYBFILE.BYB_UN10_REB_2,
      *         BYBFILE.BYB_PART_CHGOFF_1,
      *         BYBFILE.BYB_PART_CHGOFF_2,
      *         BYBFILE.BYB_ALLOTMENTS_1,
      *         BYBFILE.BYB_ALLOTMENTS_2,
      *         BYBFILE.BYB_INS_NO_ELIG_1_1,
      *         BYBFILE.BYB_INS_NO_ELIG_1_2,
      *         BYBFILE.BYB_INS_NO_ELIG_1_3,
      *         BYBFILE.BYB_INS_NO_ELIG_1_4,
      *         BYBFILE.BYB_INS_NO_ELIG_1_5,
      *         BYBFILE.BYB_INS_NO_ELIG_1_6,
      *         BYBFILE.BYB_INS_NO_ELIG_1_7,
      *         BYBFILE.BYB_INS_NO_ELIG_1_8,
      *         BYBFILE.BYB_INS_NO_ELIG_1_9,
      *         BYBFILE.BYB_INS_NO_ELIG_1_10,
      *         BYBFILE.BYB_INS_NO_ELIG_2_1,
      *         BYBFILE.BYB_INS_NO_ELIG_2_2,
      *         BYBFILE.BYB_INS_NO_ELIG_2_3,
      *         BYBFILE.BYB_INS_NO_ELIG_2_4,
      *         BYBFILE.BYB_INS_NO_ELIG_2_5,
      *         BYBFILE.BYB_INS_NO_ELIG_2_6,
      *         BYBFILE.BYB_INS_NO_ELIG_2_7,
      *         BYBFILE.BYB_INS_NO_ELIG_2_8,
      *         BYBFILE.BYB_INS_NO_ELIG_2_9,
      *         BYBFILE.BYB_INS_NO_ELIG_2_10,
      *         BYBFILE.BYB_INS_NO_WROTE_1_1,
      *         BYBFILE.BYB_INS_NO_WROTE_1_2,
      *         BYBFILE.BYB_INS_NO_WROTE_1_3,
      *         BYBFILE.BYB_INS_NO_WROTE_1_4,
      *         BYBFILE.BYB_INS_NO_WROTE_1_5,
      *         BYBFILE.BYB_INS_NO_WROTE_1_6,
      *         BYBFILE.BYB_INS_NO_WROTE_1_7,
      *         BYBFILE.BYB_INS_NO_WROTE_1_8,
      *         BYBFILE.BYB_INS_NO_WROTE_1_9,
      *         BYBFILE.BYB_INS_NO_WROTE_1_10,
      *         BYBFILE.BYB_INS_NO_WROTE_2_1,
      *         BYBFILE.BYB_INS_NO_WROTE_2_2,
      *         BYBFILE.BYB_INS_NO_WROTE_2_3,
      *         BYBFILE.BYB_INS_NO_WROTE_2_4,
      *         BYBFILE.BYB_INS_NO_WROTE_2_5,
      *         BYBFILE.BYB_INS_NO_WROTE_2_6,
      *         BYBFILE.BYB_INS_NO_WROTE_2_7,
      *         BYBFILE.BYB_INS_NO_WROTE_2_8,
      *         BYBFILE.BYB_INS_NO_WROTE_2_9,
      *         BYBFILE.BYB_INS_NO_WROTE_2_10,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_1_1,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_1_2,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_1_3,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_1_4,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_1_5,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_1_6,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_1_7,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_1_8,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_1_9,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_1_10,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_2_1,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_2_2,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_2_3,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_2_4,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_2_5,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_2_6,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_2_7,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_2_8,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_2_9,
      *         BYBFILE.BYB_INS_NO_NOTWROTE_2_10,
      *         BYBFILE.BYB_INS_AMT_WROTE_1_1,
      *         BYBFILE.BYB_INS_AMT_WROTE_1_2,
      *         BYBFILE.BYB_INS_AMT_WROTE_1_3,
      *         BYBFILE.BYB_INS_AMT_WROTE_1_4,
      *         BYBFILE.BYB_INS_AMT_WROTE_1_5,
      *         BYBFILE.BYB_INS_AMT_WROTE_1_6,
      *         BYBFILE.BYB_INS_AMT_WROTE_1_7,
      *         BYBFILE.BYB_INS_AMT_WROTE_1_8,
      *         BYBFILE.BYB_INS_AMT_WROTE_1_9,
      *         BYBFILE.BYB_INS_AMT_WROTE_1_10,
      *         BYBFILE.BYB_INS_AMT_WROTE_2_1,
      *         BYBFILE.BYB_INS_AMT_WROTE_2_2,
      *         BYBFILE.BYB_INS_AMT_WROTE_2_3,
      *         BYBFILE.BYB_INS_AMT_WROTE_2_4,
      *         BYBFILE.BYB_INS_AMT_WROTE_2_5,
      *         BYBFILE.BYB_INS_AMT_WROTE_2_6,
      *         BYBFILE.BYB_INS_AMT_WROTE_2_7,
      *         BYBFILE.BYB_INS_AMT_WROTE_2_8,
      *         BYBFILE.BYB_INS_AMT_WROTE_2_9,
      *         BYBFILE.BYB_INS_AMT_WROTE_2_10,
      *         BYBFILE.BYB_FEE_NO_ELIG_1_1,
      *         BYBFILE.BYB_FEE_NO_ELIG_1_2,
      *         BYBFILE.BYB_FEE_NO_ELIG_1_3,
      *         BYBFILE.BYB_FEE_NO_ELIG_2_1,
      *         BYBFILE.BYB_FEE_NO_ELIG_2_2,
      *         BYBFILE.BYB_FEE_NO_ELIG_2_3,
      *         BYBFILE.BYB_FEE_NO_WROTE_1_1,
      *         BYBFILE.BYB_FEE_NO_WROTE_1_2,
      *         BYBFILE.BYB_FEE_NO_WROTE_1_3,
      *         BYBFILE.BYB_FEE_NO_WROTE_2_1,
      *         BYBFILE.BYB_FEE_NO_WROTE_2_2,
      *         BYBFILE.BYB_FEE_NO_WROTE_2_3,
      *         BYBFILE.BYB_FEE_NO_NOTWROTE_1_1,
      *         BYBFILE.BYB_FEE_NO_NOTWROTE_1_2,
      *         BYBFILE.BYB_FEE_NO_NOTWROTE_1_3,
      *         BYBFILE.BYB_FEE_NO_NOTWROTE_2_1,
      *         BYBFILE.BYB_FEE_NO_NOTWROTE_2_2,
      *         BYBFILE.BYB_FEE_NO_NOTWROTE_2_3,
      *         BYBFILE.BYB_FEE_AMT_WROTE_1_1,
      *         BYBFILE.BYB_FEE_AMT_WROTE_1_2,
      *         BYBFILE.BYB_FEE_AMT_WROTE_1_3,
      *         BYBFILE.BYB_FEE_AMT_WROTE_2_1,
      *         BYBFILE.BYB_FEE_AMT_WROTE_2_2,
      *         BYBFILE.BYB_FEE_AMT_WROTE_2_3,
      *         BYBFILE.BYB_OTH_NO_COL_1,
      *         BYBFILE.BYB_OTH_NO_COL_2,
      *         BYBFILE.BYB_OT2_NO_COL_1,
      *         BYBFILE.BYB_OT2_NO_COL_2,
      *         BYBFILE.BYB_INT_NO_COL_1,
      *         BYBFILE.BYB_INT_NO_COL_2,
      *         BYBFILE.BYB_LATE_NO_COL_1,
      *         BYBFILE.BYB_LATE_NO_COL_2,
      *         BYBFILE.BYB_OTH_NO_WAIVED_1,
      *         BYBFILE.BYB_OTH_NO_WAIVED_2,
      *         BYBFILE.BYB_OT2_NO_WAIVED_1,
      *         BYBFILE.BYB_OT2_NO_WAIVED_2,
      *         BYBFILE.BYB_INT_NO_WAIVED_1,
      *         BYBFILE.BYB_INT_NO_WAIVED_2,
      *         BYBFILE.BYB_LATE_NO_WAIVED_1,
      *         BYBFILE.BYB_LATE_NO_WAIVED_2,
      *         BYBFILE.BYB_OTH_AMT_WAIVED_1,
      *         BYBFILE.BYB_OTH_AMT_WAIVED_2,
      *         BYBFILE.BYB_OT2_AMT_WAIVED_1,
      *         BYBFILE.BYB_OT2_AMT_WAIVED_2,
      *         BYBFILE.BYB_INT_AMT_WAIVED_1,
      *         BYBFILE.BYB_INT_AMT_WAIVED_2,
      *         BYBFILE.BYB_LATE_AMT_WAIVED_1,
      *         BYBFILE.BYB_LATE_AMT_WAIVED_2,
      *         BYBFILE.BYB_UNITS_1_1_1,
      *         BYBFILE.BYB_UNITS_1_1_2,
      *         BYBFILE.BYB_UNITS_1_1_3,
      *         BYBFILE.BYB_UNITS_1_1_4,
      *         BYBFILE.BYB_UNITS_1_1_5,
      *         BYBFILE.BYB_UNITS_1_1_6,
      *         BYBFILE.BYB_UNITS_1_1_7,
      *         BYBFILE.BYB_UNITS_1_1_8,
      *         BYBFILE.BYB_UNITS_1_1_9,
      *         BYBFILE.BYB_UNITS_1_1_10,
      *         BYBFILE.BYB_UNITS_1_2_1,
      *         BYBFILE.BYB_UNITS_1_2_2,
      *         BYBFILE.BYB_UNITS_1_2_3,
      *         BYBFILE.BYB_UNITS_1_2_4,
      *         BYBFILE.BYB_UNITS_1_2_5,
      *         BYBFILE.BYB_UNITS_1_2_6,
      *         BYBFILE.BYB_UNITS_1_2_7,
      *         BYBFILE.BYB_UNITS_1_2_8,
      *         BYBFILE.BYB_UNITS_1_2_9,
      *         BYBFILE.BYB_UNITS_1_2_10,
      *         BYBFILE.BYB_UNITS_2_1_1,
      *         BYBFILE.BYB_UNITS_2_1_2,
      *         BYBFILE.BYB_UNITS_2_1_3,
      *         BYBFILE.BYB_UNITS_2_1_4,
      *         BYBFILE.BYB_UNITS_2_1_5,
      *         BYBFILE.BYB_UNITS_2_1_6,
      *         BYBFILE.BYB_UNITS_2_1_7,
      *         BYBFILE.BYB_UNITS_2_1_8,
      *         BYBFILE.BYB_UNITS_2_1_9,
      *         BYBFILE.BYB_UNITS_2_1_10,
      *         BYBFILE.BYB_UNITS_2_2_1,
      *         BYBFILE.BYB_UNITS_2_2_2,
      *         BYBFILE.BYB_UNITS_2_2_3,
      *         BYBFILE.BYB_UNITS_2_2_4,
      *         BYBFILE.BYB_UNITS_2_2_5,
      *         BYBFILE.BYB_UNITS_2_2_6,
      *         BYBFILE.BYB_UNITS_2_2_7,
      *         BYBFILE.BYB_UNITS_2_2_8,
      *         BYBFILE.BYB_UNITS_2_2_9,
      *         BYBFILE.BYB_UNITS_2_2_10,
      *         BYBFILE.BYB_UNITS_3_1_1,
      *         BYBFILE.BYB_UNITS_3_1_2,
      *         BYBFILE.BYB_UNITS_3_1_3,
      *         BYBFILE.BYB_UNITS_3_1_4,
      *         BYBFILE.BYB_UNITS_3_1_5,
      *         BYBFILE.BYB_UNITS_3_1_6,
      *         BYBFILE.BYB_UNITS_3_1_7,
      *         BYBFILE.BYB_UNITS_3_1_8,
      *         BYBFILE.BYB_UNITS_3_1_9,
      *         BYBFILE.BYB_UNITS_3_1_10,
      *         BYBFILE.BYB_UNITS_3_2_1,
      *         BYBFILE.BYB_UNITS_3_2_2,
      *         BYBFILE.BYB_UNITS_3_2_3,
      *         BYBFILE.BYB_UNITS_3_2_4,
      *         BYBFILE.BYB_UNITS_3_2_5,
      *         BYBFILE.BYB_UNITS_3_2_6,
      *         BYBFILE.BYB_UNITS_3_2_7,
      *         BYBFILE.BYB_UNITS_3_2_8,
      *         BYBFILE.BYB_UNITS_3_2_9,
      *         BYBFILE.BYB_UNITS_3_2_10,
      *         BYBFILE.BYB_OUTBAL_1_1_1,
      *         BYBFILE.BYB_OUTBAL_1_1_2,
      *         BYBFILE.BYB_OUTBAL_1_1_3,
      *         BYBFILE.BYB_OUTBAL_1_1_4,
      *         BYBFILE.BYB_OUTBAL_1_1_5,
      *         BYBFILE.BYB_OUTBAL_1_1_6,
      *         BYBFILE.BYB_OUTBAL_1_1_7,
      *         BYBFILE.BYB_OUTBAL_1_1_8,
      *         BYBFILE.BYB_OUTBAL_1_1_9,
      *         BYBFILE.BYB_OUTBAL_1_1_10,
      *         BYBFILE.BYB_OUTBAL_1_2_1,
      *         BYBFILE.BYB_OUTBAL_1_2_2,
      *         BYBFILE.BYB_OUTBAL_1_2_3,
      *         BYBFILE.BYB_OUTBAL_1_2_4,
      *         BYBFILE.BYB_OUTBAL_1_2_5,
      *         BYBFILE.BYB_OUTBAL_1_2_6,
      *         BYBFILE.BYB_OUTBAL_1_2_7,
      *         BYBFILE.BYB_OUTBAL_1_2_8,
      *         BYBFILE.BYB_OUTBAL_1_2_9,
      *         BYBFILE.BYB_OUTBAL_1_2_10,
      *         BYBFILE.BYB_OUTBAL_2_1_1,
      *         BYBFILE.BYB_OUTBAL_2_1_2,
      *         BYBFILE.BYB_OUTBAL_2_1_3,
      *         BYBFILE.BYB_OUTBAL_2_1_4,
      *         BYBFILE.BYB_OUTBAL_2_1_5,
      *         BYBFILE.BYB_OUTBAL_2_1_6,
      *         BYBFILE.BYB_OUTBAL_2_1_7,
      *         BYBFILE.BYB_OUTBAL_2_1_8,
      *         BYBFILE.BYB_OUTBAL_2_1_9,
      *         BYBFILE.BYB_OUTBAL_2_1_10,
      *         BYBFILE.BYB_OUTBAL_2_2_1,
      *         BYBFILE.BYB_OUTBAL_2_2_2,
      *         BYBFILE.BYB_OUTBAL_2_2_3,
      *         BYBFILE.BYB_OUTBAL_2_2_4,
      *         BYBFILE.BYB_OUTBAL_2_2_5,
      *         BYBFILE.BYB_OUTBAL_2_2_6,
      *         BYBFILE.BYB_OUTBAL_2_2_7,
      *         BYBFILE.BYB_OUTBAL_2_2_8,
      *         BYBFILE.BYB_OUTBAL_2_2_9,
      *         BYBFILE.BYB_OUTBAL_2_2_10,
      *         BYBFILE.BYB_OUTBAL_3_1_1,
      *         BYBFILE.BYB_OUTBAL_3_1_2,
      *         BYBFILE.BYB_OUTBAL_3_1_3,
      *         BYBFILE.BYB_OUTBAL_3_1_4,
      *         BYBFILE.BYB_OUTBAL_3_1_5,
      *         BYBFILE.BYB_OUTBAL_3_1_6,
      *         BYBFILE.BYB_OUTBAL_3_1_7,
      *         BYBFILE.BYB_OUTBAL_3_1_8,
      *         BYBFILE.BYB_OUTBAL_3_1_9,
      *         BYBFILE.BYB_OUTBAL_3_1_10,
      *         BYBFILE.BYB_OUTBAL_3_2_1,
      *         BYBFILE.BYB_OUTBAL_3_2_2,
      *         BYBFILE.BYB_OUTBAL_3_2_3,
      *         BYBFILE.BYB_OUTBAL_3_2_4,
      *         BYBFILE.BYB_OUTBAL_3_2_5,
      *         BYBFILE.BYB_OUTBAL_3_2_6,
      *         BYBFILE.BYB_OUTBAL_3_2_7,
      *         BYBFILE.BYB_OUTBAL_3_2_8,
      *         BYBFILE.BYB_OUTBAL_3_2_9,
      *         BYBFILE.BYB_OUTBAL_3_2_10,
      *         BYBFILE.BYB_SUMWGTTM,
      *         BYBFILE.BYB_SUMWGTAV,
      *         BYBFILE.BYB_SUMWGTTA,
      *         BYBFILE.BYB_IB_ACCRUED,
      *         BYBFILE.BYB_DLDISC_1,
      *         BYBFILE.BYB_DLDISC_2,
      *         BYBFILE.BYB_DLDISC_3,
      *         BYBFILE.BYB_STD_POS,
      *         BYBFILE.BYB_30_DAY_MOVED,
      *         BYBFILE.BYB_PROJ_INCREASE,
      *         BYBFILE.BYB_PROJ_PROFIT,
      *         LCFILE.LC_CODE,
      *         ISNULL(LCFILE.LC_SALEFIN,' '),
      *         ISNULL(LCFILE.LC_SERVACCT,' ')
      *   FROM DBO.BYAFILE BYAFILE,
      *        DBO.BYBFILE BYBFILE
      *   LEFT OUTER JOIN
      *        DBO.LCFILE  LCFILE
      *     ON ( BYBFILE.BYB_CLASS             =  LCFILE.LC_CODE ) 
      *  WHERE ( BYAFILE.BYA_BRNO              = @QBY1_WBEG_BRNO 
      *    AND   BYAFILE.BYA_CLASS       BETWEEN @QBY1_WBEG_CLASS
      *                                      AND @QBY1_WEND_CLASS 
      *    AND   BYAFILE.BYA_DATE_CCYYMM BETWEEN @QBY1_WBEG_DATE_CCYYMM
      *                                      AND @QBY1_WEND_DATE_CCYYMM )
      *    AND ( BYAFILE.BYA_BRNO              = BYBFILE.BYB_BRNO
      *    AND   BYAFILE.BYA_CLASS             = BYBFILE.BYB_CLASS        
      *    AND   BYAFILE.BYA_DATE_CCYYMM       = BYBFILE.BYB_DATE_CCYYMM )
      *  ORDER BY BYAFILE.BYA_DATE_CCYYMM,
      *           BYAFILE.BYA_BRNO,
      *           BYAFILE.BYA_CLASS

           EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BY1_CURSOR CURSOR FOR
                :BY1-RETURN-CODE = EXEC SSP_BYANRP_BYFILE_SELECT 
              ( :QBY1-WBEG-DATE-CCYYMM, 
                :QBY1-WEND-DATE-CCYYMM,  
                :QBY1-WBEG-BRNO,        
                :QBY1-WBEG-CLASS,
                :QBY1-WEND-CLASS )
           END-EXEC.


           EXEC SQL
               OPEN BY1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBY1-WBEG-DATE-CCYYMM
                          QBY1-WEND-DATE-CCYYMM
                          QBY1-WBEG-BRNO
                          QBY1-WEND-BRNO
                          QBY1-WBEG-CLASS
                          QBY1-WEND-CLASS.

           MOVE STAT---GOOD TO BYA1-CURSOR-STAT.
		   
           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BYA1-FILE-NEXT. *> EMBEDDED - BYA,BYB,LC
           PERFORM READ-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           INITIALIZE QBYA-REC.
           INITIALIZE QBYB-REC.
           INITIALIZE QLC-REC.

           EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC.

           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BY1_CURSOR INTO  
                     :QBYA-DATE-CCYYMM,     
                     :QBYA-BRNO,
                     :QBYA-CLASS,
                     :QBYA-NO-1-1-1,
                     :QBYA-NO-1-1-2,
                     :QBYA-NO-1-1-3,
                     :QBYA-NO-1-1-4,
                     :QBYA-NO-1-1-5,
                     :QBYA-NO-1-1-6,
                     :QBYA-NO-1-1-7,
                     :QBYA-NO-1-1-8,
                     :QBYA-NO-1-1-9,
                     :QBYA-NO-1-1-10,
                     :QBYA-NO-1-1-11,
                     :QBYA-NO-1-2-1,
                     :QBYA-NO-1-2-2,
                     :QBYA-NO-1-2-3,
                     :QBYA-NO-1-2-4,
                     :QBYA-NO-1-2-5,
                     :QBYA-NO-1-2-6,
                     :QBYA-NO-1-2-7,
                     :QBYA-NO-1-2-8,
                     :QBYA-NO-1-2-9,
                     :QBYA-NO-1-2-10,
                     :QBYA-NO-1-2-11,
                     :QBYA-NO-2-1-1,
                     :QBYA-NO-2-1-2,
                     :QBYA-NO-2-1-3,
                     :QBYA-NO-2-1-4,
                     :QBYA-NO-2-1-5,
                     :QBYA-NO-2-1-6,
                     :QBYA-NO-2-1-7,
                     :QBYA-NO-2-1-8,
                     :QBYA-NO-2-1-9,
                     :QBYA-NO-2-1-10,
                     :QBYA-NO-2-1-11,
                     :QBYA-NO-2-2-1,
                     :QBYA-NO-2-2-2,
                     :QBYA-NO-2-2-3,
                     :QBYA-NO-2-2-4,
                     :QBYA-NO-2-2-5,
                     :QBYA-NO-2-2-6,
                     :QBYA-NO-2-2-7,
                     :QBYA-NO-2-2-8,
                     :QBYA-NO-2-2-9,
                     :QBYA-NO-2-2-10,
                     :QBYA-NO-2-2-11,
                     :QBYA-NO-3-1-1,
                     :QBYA-NO-3-1-2,
                     :QBYA-NO-3-1-3,
                     :QBYA-NO-3-1-4,
                     :QBYA-NO-3-1-5,
                     :QBYA-NO-3-1-6,
                     :QBYA-NO-3-1-7,
                     :QBYA-NO-3-1-8,
                     :QBYA-NO-3-1-9,
                     :QBYA-NO-3-1-10,
                     :QBYA-NO-3-1-11,
                     :QBYA-NO-3-2-1,
                     :QBYA-NO-3-2-2,
                     :QBYA-NO-3-2-3,
                     :QBYA-NO-3-2-4,
                     :QBYA-NO-3-2-5,
                     :QBYA-NO-3-2-6,
                     :QBYA-NO-3-2-7,
                     :QBYA-NO-3-2-8,
                     :QBYA-NO-3-2-9,
                     :QBYA-NO-3-2-10,
                     :QBYA-NO-3-2-11,
                     :QBYA-AMT-1-1-1,
                     :QBYA-AMT-1-1-2,
                     :QBYA-AMT-1-1-3,
                     :QBYA-AMT-1-1-4,
                     :QBYA-AMT-1-1-5,
                     :QBYA-AMT-1-1-6,
                     :QBYA-AMT-1-1-7,
                     :QBYA-AMT-1-1-8,
                     :QBYA-AMT-1-1-9,
                     :QBYA-AMT-1-1-10,
                     :QBYA-AMT-1-1-11,
                     :QBYA-AMT-1-2-1,
                     :QBYA-AMT-1-2-2,
                     :QBYA-AMT-1-2-3,
                     :QBYA-AMT-1-2-4,
                     :QBYA-AMT-1-2-5,
                     :QBYA-AMT-1-2-6,
                     :QBYA-AMT-1-2-7,
                     :QBYA-AMT-1-2-8,
                     :QBYA-AMT-1-2-9,
                     :QBYA-AMT-1-2-10,
                     :QBYA-AMT-1-2-11,
                     :QBYA-AMT-2-1-1,
                     :QBYA-AMT-2-1-2,
                     :QBYA-AMT-2-1-3,
                     :QBYA-AMT-2-1-4,
                     :QBYA-AMT-2-1-5,
                     :QBYA-AMT-2-1-6,
                     :QBYA-AMT-2-1-7,
                     :QBYA-AMT-2-1-8,
                     :QBYA-AMT-2-1-9,
                     :QBYA-AMT-2-1-10,
                     :QBYA-AMT-2-1-11,
                     :QBYA-AMT-2-2-1,
                     :QBYA-AMT-2-2-2,
                     :QBYA-AMT-2-2-3,
                     :QBYA-AMT-2-2-4,
                     :QBYA-AMT-2-2-5,
                     :QBYA-AMT-2-2-6,
                     :QBYA-AMT-2-2-7,
                     :QBYA-AMT-2-2-8,
                     :QBYA-AMT-2-2-9,
                     :QBYA-AMT-2-2-10,
                     :QBYA-AMT-2-2-11,
                     :QBYA-AMT-3-1-1,
                     :QBYA-AMT-3-1-2,
                     :QBYA-AMT-3-1-3,
                     :QBYA-AMT-3-1-4,
                     :QBYA-AMT-3-1-5,
                     :QBYA-AMT-3-1-6,
                     :QBYA-AMT-3-1-7,
                     :QBYA-AMT-3-1-8,
                     :QBYA-AMT-3-1-9,
                     :QBYA-AMT-3-1-10,
                     :QBYA-AMT-3-1-11,
                     :QBYA-AMT-3-2-1,
                     :QBYA-AMT-3-2-2,
                     :QBYA-AMT-3-2-3,
                     :QBYA-AMT-3-2-4,
                     :QBYA-AMT-3-2-5,
                     :QBYA-AMT-3-2-6,
                     :QBYA-AMT-3-2-7,
                     :QBYA-AMT-3-2-8,
                     :QBYA-AMT-3-2-9,
                     :QBYA-AMT-3-2-10,
                     :QBYA-AMT-3-2-11,
                     :QBYA-O2-1-1-1,
                     :QBYA-O2-1-1-2,
                     :QBYA-O2-1-1-3,
                     :QBYA-O2-1-1-4,
                     :QBYA-O2-1-1-5,
                     :QBYA-O2-1-1-6,
                     :QBYA-O2-1-1-7,
                     :QBYA-O2-1-1-8,
                     :QBYA-O2-1-1-9,
                     :QBYA-O2-1-1-10,
                     :QBYA-O2-1-1-11,
                     :QBYA-O2-1-2-1,
                     :QBYA-O2-1-2-2,
                     :QBYA-O2-1-2-3,
                     :QBYA-O2-1-2-4,
                     :QBYA-O2-1-2-5,
                     :QBYA-O2-1-2-6,
                     :QBYA-O2-1-2-7,
                     :QBYA-O2-1-2-8,
                     :QBYA-O2-1-2-9,
                     :QBYA-O2-1-2-10,
                     :QBYA-O2-1-2-11,
                     :QBYA-O2-2-1-1,
                     :QBYA-O2-2-1-2,
                     :QBYA-O2-2-1-3,
                     :QBYA-O2-2-1-4,
                     :QBYA-O2-2-1-5,
                     :QBYA-O2-2-1-6,
                     :QBYA-O2-2-1-7,
                     :QBYA-O2-2-1-8,
                     :QBYA-O2-2-1-9,
                     :QBYA-O2-2-1-10,
                     :QBYA-O2-2-1-11,
                     :QBYA-O2-2-2-1,
                     :QBYA-O2-2-2-2,
                     :QBYA-O2-2-2-3,
                     :QBYA-O2-2-2-4,
                     :QBYA-O2-2-2-5,
                     :QBYA-O2-2-2-6,
                     :QBYA-O2-2-2-7,
                     :QBYA-O2-2-2-8,
                     :QBYA-O2-2-2-9,
                     :QBYA-O2-2-2-10,
                     :QBYA-O2-2-2-11,
                     :QBYA-O2-3-1-1,
                     :QBYA-O2-3-1-2,
                     :QBYA-O2-3-1-3,
                     :QBYA-O2-3-1-4,
                     :QBYA-O2-3-1-5,
                     :QBYA-O2-3-1-6,
                     :QBYA-O2-3-1-7,
                     :QBYA-O2-3-1-8,
                     :QBYA-O2-3-1-9,
                     :QBYA-O2-3-1-10,
                     :QBYA-O2-3-1-11,
                     :QBYA-O2-3-2-1,
                     :QBYA-O2-3-2-2,
                     :QBYA-O2-3-2-3,
                     :QBYA-O2-3-2-4,
                     :QBYA-O2-3-2-5,
                     :QBYA-O2-3-2-6,
                     :QBYA-O2-3-2-7,
                     :QBYA-O2-3-2-8,
                     :QBYA-O2-3-2-9,
                     :QBYA-O2-3-2-10,
                     :QBYA-O2-3-2-11,
                     :QBYA-INT-1-1-1,
                     :QBYA-INT-1-1-2,
                     :QBYA-INT-1-1-3,
                     :QBYA-INT-1-1-4,
                     :QBYA-INT-1-1-5,
                     :QBYA-INT-1-1-6,
                     :QBYA-INT-1-1-7,
                     :QBYA-INT-1-1-8,
                     :QBYA-INT-1-1-9,
                     :QBYA-INT-1-1-10,
                     :QBYA-INT-1-1-11,
                     :QBYA-INT-1-2-1,
                     :QBYA-INT-1-2-2,
                     :QBYA-INT-1-2-3,
                     :QBYA-INT-1-2-4,
                     :QBYA-INT-1-2-5,
                     :QBYA-INT-1-2-6,
                     :QBYA-INT-1-2-7,
                     :QBYA-INT-1-2-8,
                     :QBYA-INT-1-2-9,
                     :QBYA-INT-1-2-10,
                     :QBYA-INT-1-2-11,
                     :QBYA-INT-2-1-1,
                     :QBYA-INT-2-1-2,
                     :QBYA-INT-2-1-3,
                     :QBYA-INT-2-1-4,
                     :QBYA-INT-2-1-5,
                     :QBYA-INT-2-1-6,
                     :QBYA-INT-2-1-7,
                     :QBYA-INT-2-1-8,
                     :QBYA-INT-2-1-9,
                     :QBYA-INT-2-1-10,
                     :QBYA-INT-2-1-11,
                     :QBYA-INT-2-2-1,
                     :QBYA-INT-2-2-2,
                     :QBYA-INT-2-2-3,
                     :QBYA-INT-2-2-4,
                     :QBYA-INT-2-2-5,
                     :QBYA-INT-2-2-6,
                     :QBYA-INT-2-2-7,
                     :QBYA-INT-2-2-8,
                     :QBYA-INT-2-2-9,
                     :QBYA-INT-2-2-10,
                     :QBYA-INT-2-2-11,
                     :QBYA-INT-3-1-1,
                     :QBYA-INT-3-1-2,
                     :QBYA-INT-3-1-3,
                     :QBYA-INT-3-1-4,
                     :QBYA-INT-3-1-5,
                     :QBYA-INT-3-1-6,
                     :QBYA-INT-3-1-7,
                     :QBYA-INT-3-1-8,
                     :QBYA-INT-3-1-9,
                     :QBYA-INT-3-1-10,
                     :QBYA-INT-3-1-11,
                     :QBYA-INT-3-2-1,
                     :QBYA-INT-3-2-2,
                     :QBYA-INT-3-2-3,
                     :QBYA-INT-3-2-4,
                     :QBYA-INT-3-2-5,
                     :QBYA-INT-3-2-6,
                     :QBYA-INT-3-2-7,
                     :QBYA-INT-3-2-8,
                     :QBYA-INT-3-2-9,
                     :QBYA-INT-3-2-10,
                     :QBYA-INT-3-2-11,
                     :QBYA-NO-REPO-1-1-1,
                     :QBYA-NO-REPO-1-1-2,
                     :QBYA-NO-REPO-1-1-3,
                     :QBYA-NO-REPO-1-1-4,
                     :QBYA-NO-REPO-1-1-5,
                     :QBYA-NO-REPO-1-2-1,
                     :QBYA-NO-REPO-1-2-2,
                     :QBYA-NO-REPO-1-2-3,
                     :QBYA-NO-REPO-1-2-4,
                     :QBYA-NO-REPO-1-2-5,
                     :QBYA-NO-REPO-2-1-1,
                     :QBYA-NO-REPO-2-1-2,
                     :QBYA-NO-REPO-2-1-3,
                     :QBYA-NO-REPO-2-1-4,
                     :QBYA-NO-REPO-2-1-5,
                     :QBYA-NO-REPO-2-2-1,
                     :QBYA-NO-REPO-2-2-2,
                     :QBYA-NO-REPO-2-2-3,
                     :QBYA-NO-REPO-2-2-4,
                     :QBYA-NO-REPO-2-2-5,
                     :QBYA-NO-REPO-3-1-1,
                     :QBYA-NO-REPO-3-1-2,
                     :QBYA-NO-REPO-3-1-3,
                     :QBYA-NO-REPO-3-1-4,
                     :QBYA-NO-REPO-3-1-5,
                     :QBYA-NO-REPO-3-2-1,
                     :QBYA-NO-REPO-3-2-2,
                     :QBYA-NO-REPO-3-2-3,
                     :QBYA-NO-REPO-3-2-4,
                     :QBYA-NO-REPO-3-2-5,
                     :QBYA-AMT-REPO-1-1-1,
                     :QBYA-AMT-REPO-1-1-2,
                     :QBYA-AMT-REPO-1-1-3,
                     :QBYA-AMT-REPO-1-1-4,
                     :QBYA-AMT-REPO-1-1-5,
                     :QBYA-AMT-REPO-1-2-1,
                     :QBYA-AMT-REPO-1-2-2,
                     :QBYA-AMT-REPO-1-2-3,
                     :QBYA-AMT-REPO-1-2-4,
                     :QBYA-AMT-REPO-1-2-5,
                     :QBYA-AMT-REPO-2-1-1,
                     :QBYA-AMT-REPO-2-1-2,
                     :QBYA-AMT-REPO-2-1-3,
                     :QBYA-AMT-REPO-2-1-4,
                     :QBYA-AMT-REPO-2-1-5,
                     :QBYA-AMT-REPO-2-2-1,
                     :QBYA-AMT-REPO-2-2-2,
                     :QBYA-AMT-REPO-2-2-3,
                     :QBYA-AMT-REPO-2-2-4,
                     :QBYA-AMT-REPO-2-2-5,
                     :QBYA-AMT-REPO-3-1-1,
                     :QBYA-AMT-REPO-3-1-2,
                     :QBYA-AMT-REPO-3-1-3,
                     :QBYA-AMT-REPO-3-1-4,
                     :QBYA-AMT-REPO-3-1-5,
                     :QBYA-AMT-REPO-3-2-1,
                     :QBYA-AMT-REPO-3-2-2,
                     :QBYA-AMT-REPO-3-2-3,
                     :QBYA-AMT-REPO-3-2-4,
                     :QBYA-AMT-REPO-3-2-5,
                     :QBYA-O2-REPO-1-1-1,
                     :QBYA-O2-REPO-1-1-2,
                     :QBYA-O2-REPO-1-1-3,
                     :QBYA-O2-REPO-1-1-4,
                     :QBYA-O2-REPO-1-1-5,
                     :QBYA-O2-REPO-1-2-1,
                     :QBYA-O2-REPO-1-2-2,
                     :QBYA-O2-REPO-1-2-3,
                     :QBYA-O2-REPO-1-2-4,
                     :QBYA-O2-REPO-1-2-5,
                     :QBYA-O2-REPO-2-1-1,
                     :QBYA-O2-REPO-2-1-2,
                     :QBYA-O2-REPO-2-1-3,
                     :QBYA-O2-REPO-2-1-4,
                     :QBYA-O2-REPO-2-1-5,
                     :QBYA-O2-REPO-2-2-1,
                     :QBYA-O2-REPO-2-2-2,
                     :QBYA-O2-REPO-2-2-3,
                     :QBYA-O2-REPO-2-2-4,
                     :QBYA-O2-REPO-2-2-5,
                     :QBYA-O2-REPO-3-1-1,
                     :QBYA-O2-REPO-3-1-2,
                     :QBYA-O2-REPO-3-1-3,
                     :QBYA-O2-REPO-3-1-4,
                     :QBYA-O2-REPO-3-1-5,
                     :QBYA-O2-REPO-3-2-1,
                     :QBYA-O2-REPO-3-2-2,
                     :QBYA-O2-REPO-3-2-3,
                     :QBYA-O2-REPO-3-2-4,
                     :QBYA-O2-REPO-3-2-5,
                     :QBYA-INT-REPO-1-1-1,
                     :QBYA-INT-REPO-1-1-2,
                     :QBYA-INT-REPO-1-1-3,
                     :QBYA-INT-REPO-1-1-4,
                     :QBYA-INT-REPO-1-1-5,
                     :QBYA-INT-REPO-1-2-1,
                     :QBYA-INT-REPO-1-2-2,
                     :QBYA-INT-REPO-1-2-3,
                     :QBYA-INT-REPO-1-2-4,
                     :QBYA-INT-REPO-1-2-5,
                     :QBYA-INT-REPO-2-1-1,
                     :QBYA-INT-REPO-2-1-2,
                     :QBYA-INT-REPO-2-1-3,
                     :QBYA-INT-REPO-2-1-4,
                     :QBYA-INT-REPO-2-1-5,
                     :QBYA-INT-REPO-2-2-1,
                     :QBYA-INT-REPO-2-2-2,
                     :QBYA-INT-REPO-2-2-3,
                     :QBYA-INT-REPO-2-2-4,
                     :QBYA-INT-REPO-2-2-5,
                     :QBYA-INT-REPO-3-1-1,
                     :QBYA-INT-REPO-3-1-2,
                     :QBYA-INT-REPO-3-1-3,
                     :QBYA-INT-REPO-3-1-4,
                     :QBYA-INT-REPO-3-1-5,
                     :QBYA-INT-REPO-3-2-1,
                     :QBYA-INT-REPO-3-2-2,
                     :QBYA-INT-REPO-3-2-3,
                     :QBYA-INT-REPO-3-2-4,
                     :QBYA-INT-REPO-3-2-5,
                     :QBYA-NO-PL-1-1-1,
                     :QBYA-NO-PL-1-1-2,
                     :QBYA-NO-PL-1-1-3,
                     :QBYA-NO-PL-1-1-4,
                     :QBYA-NO-PL-1-1-5,
                     :QBYA-NO-PL-1-1-6,
                     :QBYA-NO-PL-1-1-7,
                     :QBYA-NO-PL-1-1-8,
                     :QBYA-NO-PL-1-1-9,
                     :QBYA-NO-PL-1-1-10,
                     :QBYA-NO-PL-1-2-1,
                     :QBYA-NO-PL-1-2-2,
                     :QBYA-NO-PL-1-2-3,
                     :QBYA-NO-PL-1-2-4,
                     :QBYA-NO-PL-1-2-5,
                     :QBYA-NO-PL-1-2-6,
                     :QBYA-NO-PL-1-2-7,
                     :QBYA-NO-PL-1-2-8,
                     :QBYA-NO-PL-1-2-9,
                     :QBYA-NO-PL-1-2-10,
                     :QBYA-NO-PL-2-1-1,
                     :QBYA-NO-PL-2-1-2,
                     :QBYA-NO-PL-2-1-3,
                     :QBYA-NO-PL-2-1-4,
                     :QBYA-NO-PL-2-1-5,
                     :QBYA-NO-PL-2-1-6,
                     :QBYA-NO-PL-2-1-7,
                     :QBYA-NO-PL-2-1-8,
                     :QBYA-NO-PL-2-1-9,
                     :QBYA-NO-PL-2-1-10,
                     :QBYA-NO-PL-2-2-1,
                     :QBYA-NO-PL-2-2-2,
                     :QBYA-NO-PL-2-2-3,
                     :QBYA-NO-PL-2-2-4,
                     :QBYA-NO-PL-2-2-5,
                     :QBYA-NO-PL-2-2-6,
                     :QBYA-NO-PL-2-2-7,
                     :QBYA-NO-PL-2-2-8,
                     :QBYA-NO-PL-2-2-9,
                     :QBYA-NO-PL-2-2-10,
                     :QBYA-NO-PL-3-1-1,
                     :QBYA-NO-PL-3-1-2,
                     :QBYA-NO-PL-3-1-3,
                     :QBYA-NO-PL-3-1-4,
                     :QBYA-NO-PL-3-1-5,
                     :QBYA-NO-PL-3-1-6,
                     :QBYA-NO-PL-3-1-7,
                     :QBYA-NO-PL-3-1-8,
                     :QBYA-NO-PL-3-1-9,
                     :QBYA-NO-PL-3-1-10,
                     :QBYA-NO-PL-3-2-1,
                     :QBYA-NO-PL-3-2-2,
                     :QBYA-NO-PL-3-2-3,
                     :QBYA-NO-PL-3-2-4,
                     :QBYA-NO-PL-3-2-5,
                     :QBYA-NO-PL-3-2-6,
                     :QBYA-NO-PL-3-2-7,
                     :QBYA-NO-PL-3-2-8,
                     :QBYA-NO-PL-3-2-9,
                     :QBYA-NO-PL-3-2-10,
                     :QBYA-AMT-PL-1-1-1,
                     :QBYA-AMT-PL-1-1-2,
                     :QBYA-AMT-PL-1-1-3,
                     :QBYA-AMT-PL-1-1-4,
                     :QBYA-AMT-PL-1-1-5,
                     :QBYA-AMT-PL-1-1-6,
                     :QBYA-AMT-PL-1-1-7,
                     :QBYA-AMT-PL-1-1-8,
                     :QBYA-AMT-PL-1-1-9,
                     :QBYA-AMT-PL-1-1-10,
                     :QBYA-AMT-PL-1-2-1,
                     :QBYA-AMT-PL-1-2-2,
                     :QBYA-AMT-PL-1-2-3,
                     :QBYA-AMT-PL-1-2-4,
                     :QBYA-AMT-PL-1-2-5,
                     :QBYA-AMT-PL-1-2-6,
                     :QBYA-AMT-PL-1-2-7,
                     :QBYA-AMT-PL-1-2-8,
                     :QBYA-AMT-PL-1-2-9,
                     :QBYA-AMT-PL-1-2-10,
                     :QBYA-AMT-PL-2-1-1,
                     :QBYA-AMT-PL-2-1-2,
                     :QBYA-AMT-PL-2-1-3,
                     :QBYA-AMT-PL-2-1-4,
                     :QBYA-AMT-PL-2-1-5,
                     :QBYA-AMT-PL-2-1-6,
                     :QBYA-AMT-PL-2-1-7,
                     :QBYA-AMT-PL-2-1-8,
                     :QBYA-AMT-PL-2-1-9,
                     :QBYA-AMT-PL-2-1-10,
                     :QBYA-AMT-PL-2-2-1,
                     :QBYA-AMT-PL-2-2-2,
                     :QBYA-AMT-PL-2-2-3,
                     :QBYA-AMT-PL-2-2-4,
                     :QBYA-AMT-PL-2-2-5,
                     :QBYA-AMT-PL-2-2-6,
                     :QBYA-AMT-PL-2-2-7,
                     :QBYA-AMT-PL-2-2-8,
                     :QBYA-AMT-PL-2-2-9,
                     :QBYA-AMT-PL-2-2-10,
                     :QBYA-AMT-PL-3-1-1,
                     :QBYA-AMT-PL-3-1-2,
                     :QBYA-AMT-PL-3-1-3,
                     :QBYA-AMT-PL-3-1-4,
                     :QBYA-AMT-PL-3-1-5,
                     :QBYA-AMT-PL-3-1-6,
                     :QBYA-AMT-PL-3-1-7,
                     :QBYA-AMT-PL-3-1-8,
                     :QBYA-AMT-PL-3-1-9,
                     :QBYA-AMT-PL-3-1-10,
                     :QBYA-AMT-PL-3-2-1,
                     :QBYA-AMT-PL-3-2-2,
                     :QBYA-AMT-PL-3-2-3,
                     :QBYA-AMT-PL-3-2-4,
                     :QBYA-AMT-PL-3-2-5,
                     :QBYA-AMT-PL-3-2-6,
                     :QBYA-AMT-PL-3-2-7,
                     :QBYA-AMT-PL-3-2-8,
                     :QBYA-AMT-PL-3-2-9,
                     :QBYA-AMT-PL-3-2-10,
                     :QBYA-O2-PL-1-1-1,
                     :QBYA-O2-PL-1-1-2,
                     :QBYA-O2-PL-1-1-3,
                     :QBYA-O2-PL-1-1-4,
                     :QBYA-O2-PL-1-1-5,
                     :QBYA-O2-PL-1-1-6,
                     :QBYA-O2-PL-1-1-7,
                     :QBYA-O2-PL-1-1-8,
                     :QBYA-O2-PL-1-1-9,
                     :QBYA-O2-PL-1-1-10,
                     :QBYA-O2-PL-1-2-1,
                     :QBYA-O2-PL-1-2-2,
                     :QBYA-O2-PL-1-2-3,
                     :QBYA-O2-PL-1-2-4,
                     :QBYA-O2-PL-1-2-5,
                     :QBYA-O2-PL-1-2-6,
                     :QBYA-O2-PL-1-2-7,
                     :QBYA-O2-PL-1-2-8,
                     :QBYA-O2-PL-1-2-9,
                     :QBYA-O2-PL-1-2-10,
                     :QBYA-O2-PL-2-1-1,
                     :QBYA-O2-PL-2-1-2,
                     :QBYA-O2-PL-2-1-3,
                     :QBYA-O2-PL-2-1-4,
                     :QBYA-O2-PL-2-1-5,
                     :QBYA-O2-PL-2-1-6,
                     :QBYA-O2-PL-2-1-7,
                     :QBYA-O2-PL-2-1-8,
                     :QBYA-O2-PL-2-1-9,
                     :QBYA-O2-PL-2-1-10,
                     :QBYA-O2-PL-2-2-1,
                     :QBYA-O2-PL-2-2-2,
                     :QBYA-O2-PL-2-2-3,
                     :QBYA-O2-PL-2-2-4,
                     :QBYA-O2-PL-2-2-5,
                     :QBYA-O2-PL-2-2-6,
                     :QBYA-O2-PL-2-2-7,
                     :QBYA-O2-PL-2-2-8,
                     :QBYA-O2-PL-2-2-9,
                     :QBYA-O2-PL-2-2-10,
                     :QBYA-O2-PL-3-1-1,
                     :QBYA-O2-PL-3-1-2,
                     :QBYA-O2-PL-3-1-3,
                     :QBYA-O2-PL-3-1-4,
                     :QBYA-O2-PL-3-1-5,
                     :QBYA-O2-PL-3-1-6,
                     :QBYA-O2-PL-3-1-7,
                     :QBYA-O2-PL-3-1-8,
                     :QBYA-O2-PL-3-1-9,
                     :QBYA-O2-PL-3-1-10,
                     :QBYA-O2-PL-3-2-1,
                     :QBYA-O2-PL-3-2-2,
                     :QBYA-O2-PL-3-2-3,
                     :QBYA-O2-PL-3-2-4,
                     :QBYA-O2-PL-3-2-5,
                     :QBYA-O2-PL-3-2-6,
                     :QBYA-O2-PL-3-2-7,
                     :QBYA-O2-PL-3-2-8,
                     :QBYA-O2-PL-3-2-9,
                     :QBYA-O2-PL-3-2-10,
                     :QBYA-INT-PL-1-1-1,
                     :QBYA-INT-PL-1-1-2,
                     :QBYA-INT-PL-1-1-3,
                     :QBYA-INT-PL-1-1-4,
                     :QBYA-INT-PL-1-1-5,
                     :QBYA-INT-PL-1-1-6,
                     :QBYA-INT-PL-1-1-7,
                     :QBYA-INT-PL-1-1-8,
                     :QBYA-INT-PL-1-1-9,
                     :QBYA-INT-PL-1-1-10,
                     :QBYA-INT-PL-1-2-1,
                     :QBYA-INT-PL-1-2-2,
                     :QBYA-INT-PL-1-2-3,
                     :QBYA-INT-PL-1-2-4,
                     :QBYA-INT-PL-1-2-5,
                     :QBYA-INT-PL-1-2-6,
                     :QBYA-INT-PL-1-2-7,
                     :QBYA-INT-PL-1-2-8,
                     :QBYA-INT-PL-1-2-9,
                     :QBYA-INT-PL-1-2-10,
                     :QBYA-INT-PL-2-1-1,
                     :QBYA-INT-PL-2-1-2,
                     :QBYA-INT-PL-2-1-3,
                     :QBYA-INT-PL-2-1-4,
                     :QBYA-INT-PL-2-1-5,
                     :QBYA-INT-PL-2-1-6,
                     :QBYA-INT-PL-2-1-7,
                     :QBYA-INT-PL-2-1-8,
                     :QBYA-INT-PL-2-1-9,
                     :QBYA-INT-PL-2-1-10,
                     :QBYA-INT-PL-2-2-1,
                     :QBYA-INT-PL-2-2-2,
                     :QBYA-INT-PL-2-2-3,
                     :QBYA-INT-PL-2-2-4,
                     :QBYA-INT-PL-2-2-5,
                     :QBYA-INT-PL-2-2-6,
                     :QBYA-INT-PL-2-2-7,
                     :QBYA-INT-PL-2-2-8,
                     :QBYA-INT-PL-2-2-9,
                     :QBYA-INT-PL-2-2-10,
                     :QBYA-INT-PL-3-1-1,
                     :QBYA-INT-PL-3-1-2,
                     :QBYA-INT-PL-3-1-3,
                     :QBYA-INT-PL-3-1-4,
                     :QBYA-INT-PL-3-1-5,
                     :QBYA-INT-PL-3-1-6,
                     :QBYA-INT-PL-3-1-7,
                     :QBYA-INT-PL-3-1-8,
                     :QBYA-INT-PL-3-1-9,
                     :QBYA-INT-PL-3-1-10,
                     :QBYA-INT-PL-3-2-1,
                     :QBYA-INT-PL-3-2-2,
                     :QBYA-INT-PL-3-2-3,
                     :QBYA-INT-PL-3-2-4,
                     :QBYA-INT-PL-3-2-5,
                     :QBYA-INT-PL-3-2-6,
                     :QBYA-INT-PL-3-2-7,
                     :QBYA-INT-PL-3-2-8,
                     :QBYA-INT-PL-3-2-9,
                     :QBYA-INT-PL-3-2-10,
                     :QBYA-DLRVPRIN-1-1-1,
                     :QBYA-DLRVPRIN-1-1-2,
                     :QBYA-DLRVPRIN-1-1-3,
                     :QBYA-DLRVPRIN-1-2-1,
                     :QBYA-DLRVPRIN-1-2-2,
                     :QBYA-DLRVPRIN-1-2-3,
                     :QBYA-DLRVPRIN-2-1-1,
                     :QBYA-DLRVPRIN-2-1-2,
                     :QBYA-DLRVPRIN-2-1-3,
                     :QBYA-DLRVPRIN-2-2-1,
                     :QBYA-DLRVPRIN-2-2-2,
                     :QBYA-DLRVPRIN-2-2-3,
                     :QBYA-DLRVPRIN-3-1-1,
                     :QBYA-DLRVPRIN-3-1-2,
                     :QBYA-DLRVPRIN-3-1-3,
                     :QBYA-DLRVPRIN-3-2-1,
                     :QBYA-DLRVPRIN-3-2-2,
                     :QBYA-DLRVPRIN-3-2-3,
                     :QBYA-DLRVCHGS-1-1-1,
                     :QBYA-DLRVCHGS-1-1-2,
                     :QBYA-DLRVCHGS-1-1-3,
                     :QBYA-DLRVCHGS-1-2-1,
                     :QBYA-DLRVCHGS-1-2-2,
                     :QBYA-DLRVCHGS-1-2-3,
                     :QBYA-DLRVCHGS-2-1-1,
                     :QBYA-DLRVCHGS-2-1-2,
                     :QBYA-DLRVCHGS-2-1-3,
                     :QBYA-DLRVCHGS-2-2-1,
                     :QBYA-DLRVCHGS-2-2-2,
                     :QBYA-DLRVCHGS-2-2-3,
                     :QBYA-DLRVCHGS-3-1-1,
                     :QBYA-DLRVCHGS-3-1-2,
                     :QBYA-DLRVCHGS-3-1-3,
                     :QBYA-DLRVCHGS-3-2-1,
                     :QBYA-DLRVCHGS-3-2-2,
                     :QBYA-DLRVCHGS-3-2-3,
                     :QBYA-DLPARVHELD-1-1,
                     :QBYA-DLPARVHELD-1-2,
                     :QBYA-DLPARVHELD-2-1,
                     :QBYA-DLPARVHELD-2-2,
                     :QBYA-DLPARVHELD-3-1,
                     :QBYA-DLPARVHELD-3-2,
                     :QBYB-PL-PRINCOL-1-1,
                     :QBYB-PL-PRINCOL-1-2,
                     :QBYB-PL-PRINCOL-1-3,
                     :QBYB-PL-PRINCOL-1-4,
                     :QBYB-PL-PRINCOL-1-5,
                     :QBYB-PL-PRINCOL-2-1,
                     :QBYB-PL-PRINCOL-2-2,
                     :QBYB-PL-PRINCOL-2-3,
                     :QBYB-PL-PRINCOL-2-4,
                     :QBYB-PL-PRINCOL-2-5,
                     :QBYB-PL-INTCOL-1-1,
                     :QBYB-PL-INTCOL-1-2,
                     :QBYB-PL-INTCOL-1-3,
                     :QBYB-PL-INTCOL-1-4,
                     :QBYB-PL-INTCOL-1-5,
                     :QBYB-PL-INTCOL-2-1,
                     :QBYB-PL-INTCOL-2-2,
                     :QBYB-PL-INTCOL-2-3,
                     :QBYB-PL-INTCOL-2-4,
                     :QBYB-PL-INTCOL-2-5,
                     :QBYB-PL-LATECOL-1-1,
                     :QBYB-PL-LATECOL-1-2,
                     :QBYB-PL-LATECOL-1-3,
                     :QBYB-PL-LATECOL-1-4,
                     :QBYB-PL-LATECOL-1-5,
                     :QBYB-PL-LATECOL-2-1,
                     :QBYB-PL-LATECOL-2-2,
                     :QBYB-PL-LATECOL-2-3,
                     :QBYB-PL-LATECOL-2-4,
                     :QBYB-PL-LATECOL-2-5,
                     :QBYB-PL-O2COL-1-1,
                     :QBYB-PL-O2COL-1-2,
                     :QBYB-PL-O2COL-1-3,
                     :QBYB-PL-O2COL-1-4,
                     :QBYB-PL-O2COL-1-5,
                     :QBYB-PL-O2COL-2-1,
                     :QBYB-PL-O2COL-2-2,
                     :QBYB-PL-O2COL-2-3,
                     :QBYB-PL-O2COL-2-4,
                     :QBYB-PL-O2COL-2-5,
                     :QBYB-AMT-SOLD-1,
                     :QBYB-AMT-SOLD-2,
                     :QBYB-AMT-TRANS-1,
                     :QBYB-AMT-TRANS-2,
                     :QBYB-AMT-PURCH-1,
                     :QBYB-AMT-PURCH-2,
                     :QBYB-NO-SOLD-1,
                     :QBYB-NO-SOLD-2,
                     :QBYB-NO-TRANS-1,
                     :QBYB-NO-TRANS-2,
                     :QBYB-NO-PURCH-1,
                     :QBYB-NO-PURCH-2,
                     :QBYB-OTHCOL-1,
                     :QBYB-OTHCOL-2,
                     :QBYB-OT2COL-1,
                     :QBYB-OT2COL-2,
                     :QBYB-INTCOL-1,
                     :QBYB-INTCOL-2,
                     :QBYB-LATECOL-1,
                     :QBYB-LATECOL-2,
                     :QBYB-PRINCOL-1,
                     :QBYB-PRINCOL-2,
                     :QBYB-DEFCOL-1,
                     :QBYB-DEFCOL-2,
                     :QBYB-RNPRIN-1,
                     :QBYB-RNPRIN-2,
                     :QBYB-INTREB-1,
                     :QBYB-INTREB-2,
                     :QBYB-CLREB1-1,
                     :QBYB-CLREB1-2,
                     :QBYB-CLREB2-1,
                     :QBYB-CLREB2-2,
                     :QBYB-CLREB3-1,
                     :QBYB-CLREB3-2,
                     :QBYB-CLREB4-1,
                     :QBYB-CLREB4-2,
                     :QBYB-AHREB-1,
                     :QBYB-AHREB-2,
                     :QBYB-PPREB-1,
                     :QBYB-PPREB-2,
                     :QBYB-O1REB-1,
                     :QBYB-O1REB-2,
                     :QBYB-O2REB-1,
                     :QBYB-O2REB-2,
                     :QBYB-O3REB-1,
                     :QBYB-O3REB-2,
                     :QBYB-O4REB-1,
                     :QBYB-O4REB-2,
                     :QBYB-O5REB-1,
                     :QBYB-O5REB-2,
                     :QBYB-O6REB-1,
                     :QBYB-O6REB-2,
                     :QBYB-O7REB-1,
                     :QBYB-O7REB-2,
                     :QBYB-SCREB-1,
                     :QBYB-SCREB-2,
                     :QBYB-MFREB-1,
                     :QBYB-MFREB-2,
                     :QBYB-DFREB-1,
                     :QBYB-DFREB-2,
                     :QBYB-UN7-REB-1,
                     :QBYB-UN7-REB-2,
                     :QBYB-UN8-REB-1,
                     :QBYB-UN8-REB-2,
                     :QBYB-UN9-REB-1,
                     :QBYB-UN9-REB-2,
                     :QBYB-UN10-REB-1,
                     :QBYB-UN10-REB-2,
                     :QBYB-PART-CHGOFF-1,
                     :QBYB-PART-CHGOFF-2,
                     :QBYB-ALLOTMENTS-1,
                     :QBYB-ALLOTMENTS-2,
                     :QBYB-INS-NO-ELIG-1-1,
                     :QBYB-INS-NO-ELIG-1-2,
                     :QBYB-INS-NO-ELIG-1-3,
                     :QBYB-INS-NO-ELIG-1-4,
                     :QBYB-INS-NO-ELIG-1-5,
                     :QBYB-INS-NO-ELIG-1-6,
                     :QBYB-INS-NO-ELIG-1-7,
                     :QBYB-INS-NO-ELIG-1-8,
                     :QBYB-INS-NO-ELIG-1-9,
                     :QBYB-INS-NO-ELIG-1-10,
                     :QBYB-INS-NO-ELIG-2-1,
                     :QBYB-INS-NO-ELIG-2-2,
                     :QBYB-INS-NO-ELIG-2-3,
                     :QBYB-INS-NO-ELIG-2-4,
                     :QBYB-INS-NO-ELIG-2-5,
                     :QBYB-INS-NO-ELIG-2-6,
                     :QBYB-INS-NO-ELIG-2-7,
                     :QBYB-INS-NO-ELIG-2-8,
                     :QBYB-INS-NO-ELIG-2-9,
                     :QBYB-INS-NO-ELIG-2-10,
                     :QBYB-INS-NO-WROTE-1-1,
                     :QBYB-INS-NO-WROTE-1-2,
                     :QBYB-INS-NO-WROTE-1-3,
                     :QBYB-INS-NO-WROTE-1-4,
                     :QBYB-INS-NO-WROTE-1-5,
                     :QBYB-INS-NO-WROTE-1-6,
                     :QBYB-INS-NO-WROTE-1-7,
                     :QBYB-INS-NO-WROTE-1-8,
                     :QBYB-INS-NO-WROTE-1-9,
                     :QBYB-INS-NO-WROTE-1-10,
                     :QBYB-INS-NO-WROTE-2-1,
                     :QBYB-INS-NO-WROTE-2-2,
                     :QBYB-INS-NO-WROTE-2-3,
                     :QBYB-INS-NO-WROTE-2-4,
                     :QBYB-INS-NO-WROTE-2-5,
                     :QBYB-INS-NO-WROTE-2-6,
                     :QBYB-INS-NO-WROTE-2-7,
                     :QBYB-INS-NO-WROTE-2-8,
                     :QBYB-INS-NO-WROTE-2-9,
                     :QBYB-INS-NO-WROTE-2-10,
                     :QBYB-INS-NO-NOTWROTE-1-1,
                     :QBYB-INS-NO-NOTWROTE-1-2,
                     :QBYB-INS-NO-NOTWROTE-1-3,
                     :QBYB-INS-NO-NOTWROTE-1-4,
                     :QBYB-INS-NO-NOTWROTE-1-5,
                     :QBYB-INS-NO-NOTWROTE-1-6,
                     :QBYB-INS-NO-NOTWROTE-1-7,
                     :QBYB-INS-NO-NOTWROTE-1-8,
                     :QBYB-INS-NO-NOTWROTE-1-9,
                     :QBYB-INS-NO-NOTWROTE-1-10,
                     :QBYB-INS-NO-NOTWROTE-2-1,
                     :QBYB-INS-NO-NOTWROTE-2-2,
                     :QBYB-INS-NO-NOTWROTE-2-3,
                     :QBYB-INS-NO-NOTWROTE-2-4,
                     :QBYB-INS-NO-NOTWROTE-2-5,
                     :QBYB-INS-NO-NOTWROTE-2-6,
                     :QBYB-INS-NO-NOTWROTE-2-7,
                     :QBYB-INS-NO-NOTWROTE-2-8,
                     :QBYB-INS-NO-NOTWROTE-2-9,
                     :QBYB-INS-NO-NOTWROTE-2-10,
                     :QBYB-INS-AMT-WROTE-1-1,
                     :QBYB-INS-AMT-WROTE-1-2,
                     :QBYB-INS-AMT-WROTE-1-3,
                     :QBYB-INS-AMT-WROTE-1-4,
                     :QBYB-INS-AMT-WROTE-1-5,
                     :QBYB-INS-AMT-WROTE-1-6,
                     :QBYB-INS-AMT-WROTE-1-7,
                     :QBYB-INS-AMT-WROTE-1-8,
                     :QBYB-INS-AMT-WROTE-1-9,
                     :QBYB-INS-AMT-WROTE-1-10,
                     :QBYB-INS-AMT-WROTE-2-1,
                     :QBYB-INS-AMT-WROTE-2-2,
                     :QBYB-INS-AMT-WROTE-2-3,
                     :QBYB-INS-AMT-WROTE-2-4,
                     :QBYB-INS-AMT-WROTE-2-5,
                     :QBYB-INS-AMT-WROTE-2-6,
                     :QBYB-INS-AMT-WROTE-2-7,
                     :QBYB-INS-AMT-WROTE-2-8,
                     :QBYB-INS-AMT-WROTE-2-9,
                     :QBYB-INS-AMT-WROTE-2-10,
                     :QBYB-FEE-NO-ELIG-1-1,
                     :QBYB-FEE-NO-ELIG-1-2,
                     :QBYB-FEE-NO-ELIG-1-3,
                     :QBYB-FEE-NO-ELIG-2-1,
                     :QBYB-FEE-NO-ELIG-2-2,
                     :QBYB-FEE-NO-ELIG-2-3,
                     :QBYB-FEE-NO-WROTE-1-1,
                     :QBYB-FEE-NO-WROTE-1-2,
                     :QBYB-FEE-NO-WROTE-1-3,
                     :QBYB-FEE-NO-WROTE-2-1,
                     :QBYB-FEE-NO-WROTE-2-2,
                     :QBYB-FEE-NO-WROTE-2-3,
                     :QBYB-FEE-NO-NOTWROTE-1-1,
                     :QBYB-FEE-NO-NOTWROTE-1-2,
                     :QBYB-FEE-NO-NOTWROTE-1-3,
                     :QBYB-FEE-NO-NOTWROTE-2-1,
                     :QBYB-FEE-NO-NOTWROTE-2-2,
                     :QBYB-FEE-NO-NOTWROTE-2-3,
                     :QBYB-FEE-AMT-WROTE-1-1,
                     :QBYB-FEE-AMT-WROTE-1-2,
                     :QBYB-FEE-AMT-WROTE-1-3,
                     :QBYB-FEE-AMT-WROTE-2-1,
                     :QBYB-FEE-AMT-WROTE-2-2,
                     :QBYB-FEE-AMT-WROTE-2-3,
                     :QBYB-OTH-NO-COL-1,
                     :QBYB-OTH-NO-COL-2,
                     :QBYB-OT2-NO-COL-1,
                     :QBYB-OT2-NO-COL-2,
                     :QBYB-INT-NO-COL-1,
                     :QBYB-INT-NO-COL-2,
                     :QBYB-LATE-NO-COL-1,
                     :QBYB-LATE-NO-COL-2,
                     :QBYB-OTH-NO-WAIVED-1,
                     :QBYB-OTH-NO-WAIVED-2,
                     :QBYB-OT2-NO-WAIVED-1,
                     :QBYB-OT2-NO-WAIVED-2,
                     :QBYB-INT-NO-WAIVED-1,
                     :QBYB-INT-NO-WAIVED-2,
                     :QBYB-LATE-NO-WAIVED-1,
                     :QBYB-LATE-NO-WAIVED-2,
                     :QBYB-OTH-AMT-WAIVED-1,
                     :QBYB-OTH-AMT-WAIVED-2,
                     :QBYB-OT2-AMT-WAIVED-1,
                     :QBYB-OT2-AMT-WAIVED-2,
                     :QBYB-INT-AMT-WAIVED-1,
                     :QBYB-INT-AMT-WAIVED-2,
                     :QBYB-LATE-AMT-WAIVED-1,
                     :QBYB-LATE-AMT-WAIVED-2,
                     :QBYB-UNITS-1-1-1,
                     :QBYB-UNITS-1-1-2,
                     :QBYB-UNITS-1-1-3,
                     :QBYB-UNITS-1-1-4,
                     :QBYB-UNITS-1-1-5,
                     :QBYB-UNITS-1-1-6,
                     :QBYB-UNITS-1-1-7,
                     :QBYB-UNITS-1-1-8,
                     :QBYB-UNITS-1-1-9,
                     :QBYB-UNITS-1-1-10,
                     :QBYB-UNITS-1-2-1,
                     :QBYB-UNITS-1-2-2,
                     :QBYB-UNITS-1-2-3,
                     :QBYB-UNITS-1-2-4,
                     :QBYB-UNITS-1-2-5,
                     :QBYB-UNITS-1-2-6,
                     :QBYB-UNITS-1-2-7,
                     :QBYB-UNITS-1-2-8,
                     :QBYB-UNITS-1-2-9,
                     :QBYB-UNITS-1-2-10,
                     :QBYB-UNITS-2-1-1,
                     :QBYB-UNITS-2-1-2,
                     :QBYB-UNITS-2-1-3,
                     :QBYB-UNITS-2-1-4,
                     :QBYB-UNITS-2-1-5,
                     :QBYB-UNITS-2-1-6,
                     :QBYB-UNITS-2-1-7,
                     :QBYB-UNITS-2-1-8,
                     :QBYB-UNITS-2-1-9,
                     :QBYB-UNITS-2-1-10,
                     :QBYB-UNITS-2-2-1,
                     :QBYB-UNITS-2-2-2,
                     :QBYB-UNITS-2-2-3,
                     :QBYB-UNITS-2-2-4,
                     :QBYB-UNITS-2-2-5,
                     :QBYB-UNITS-2-2-6,
                     :QBYB-UNITS-2-2-7,
                     :QBYB-UNITS-2-2-8,
                     :QBYB-UNITS-2-2-9,
                     :QBYB-UNITS-2-2-10,
                     :QBYB-UNITS-3-1-1,
                     :QBYB-UNITS-3-1-2,
                     :QBYB-UNITS-3-1-3,
                     :QBYB-UNITS-3-1-4,
                     :QBYB-UNITS-3-1-5,
                     :QBYB-UNITS-3-1-6,
                     :QBYB-UNITS-3-1-7,
                     :QBYB-UNITS-3-1-8,
                     :QBYB-UNITS-3-1-9,
                     :QBYB-UNITS-3-1-10,
                     :QBYB-UNITS-3-2-1,
                     :QBYB-UNITS-3-2-2,
                     :QBYB-UNITS-3-2-3,
                     :QBYB-UNITS-3-2-4,
                     :QBYB-UNITS-3-2-5,
                     :QBYB-UNITS-3-2-6,
                     :QBYB-UNITS-3-2-7,
                     :QBYB-UNITS-3-2-8,
                     :QBYB-UNITS-3-2-9,
                     :QBYB-UNITS-3-2-10,
                     :QBYB-OUTBAL-1-1-1,
                     :QBYB-OUTBAL-1-1-2,
                     :QBYB-OUTBAL-1-1-3,
                     :QBYB-OUTBAL-1-1-4,
                     :QBYB-OUTBAL-1-1-5,
                     :QBYB-OUTBAL-1-1-6,
                     :QBYB-OUTBAL-1-1-7,
                     :QBYB-OUTBAL-1-1-8,
                     :QBYB-OUTBAL-1-1-9,
                     :QBYB-OUTBAL-1-1-10,
                     :QBYB-OUTBAL-1-2-1,
                     :QBYB-OUTBAL-1-2-2,
                     :QBYB-OUTBAL-1-2-3,
                     :QBYB-OUTBAL-1-2-4,
                     :QBYB-OUTBAL-1-2-5,
                     :QBYB-OUTBAL-1-2-6,
                     :QBYB-OUTBAL-1-2-7,
                     :QBYB-OUTBAL-1-2-8,
                     :QBYB-OUTBAL-1-2-9,
                     :QBYB-OUTBAL-1-2-10,
                     :QBYB-OUTBAL-2-1-1,
                     :QBYB-OUTBAL-2-1-2,
                     :QBYB-OUTBAL-2-1-3,
                     :QBYB-OUTBAL-2-1-4,
                     :QBYB-OUTBAL-2-1-5,
                     :QBYB-OUTBAL-2-1-6,
                     :QBYB-OUTBAL-2-1-7,
                     :QBYB-OUTBAL-2-1-8,
                     :QBYB-OUTBAL-2-1-9,
                     :QBYB-OUTBAL-2-1-10,
                     :QBYB-OUTBAL-2-2-1,
                     :QBYB-OUTBAL-2-2-2,
                     :QBYB-OUTBAL-2-2-3,
                     :QBYB-OUTBAL-2-2-4,
                     :QBYB-OUTBAL-2-2-5,
                     :QBYB-OUTBAL-2-2-6,
                     :QBYB-OUTBAL-2-2-7,
                     :QBYB-OUTBAL-2-2-8,
                     :QBYB-OUTBAL-2-2-9,
                     :QBYB-OUTBAL-2-2-10,
                     :QBYB-OUTBAL-3-1-1,
                     :QBYB-OUTBAL-3-1-2,
                     :QBYB-OUTBAL-3-1-3,
                     :QBYB-OUTBAL-3-1-4,
                     :QBYB-OUTBAL-3-1-5,
                     :QBYB-OUTBAL-3-1-6,
                     :QBYB-OUTBAL-3-1-7,
                     :QBYB-OUTBAL-3-1-8,
                     :QBYB-OUTBAL-3-1-9,
                     :QBYB-OUTBAL-3-1-10,
                     :QBYB-OUTBAL-3-2-1,
                     :QBYB-OUTBAL-3-2-2,
                     :QBYB-OUTBAL-3-2-3,
                     :QBYB-OUTBAL-3-2-4,
                     :QBYB-OUTBAL-3-2-5,
                     :QBYB-OUTBAL-3-2-6,
                     :QBYB-OUTBAL-3-2-7,
                     :QBYB-OUTBAL-3-2-8,
                     :QBYB-OUTBAL-3-2-9,
                     :QBYB-OUTBAL-3-2-10,
                     :QBYB-SUMWGTTM,
                     :QBYB-SUMWGTAV,
                     :QBYB-SUMWGTTA,
                     :QBYB-IB-ACCRUED,
                     :QBYB-DLDISC-1,
                     :QBYB-DLDISC-2,
                     :QBYB-DLDISC-3,
                     :QBYB-STD-POS,
                     :QBYB-30-DAY-MOVED,
                     :QBYB-PROJ-INCREASE,
                     :QBYB-PROJ-PROFIT,
                     :QLC-CODE,
                     :QLC-SALEFIN, 
                     :QLC-SERVACCT 
                 END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BYA-FIELDS
              PERFORM GET-BYB-FIELDS 
              PERFORM GET-LC-FIELDS.
			  
          EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BYA1-FILE.
           PERFORM CLOSE-IT.

           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BY1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO BYA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-BY1-CONNECTION.

           IF BY1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT BY1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO BY1-CONN-OPEN-SW.


       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

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
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BYANRP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.


