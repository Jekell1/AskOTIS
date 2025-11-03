       IDENTIFICATION DIVISION.
       PROGRAM-ID.      UMCEXT.
       DATE-WRITTEN.    102904.
      *****************************************************************
      *                   UNITED MOTOR CLUB
      *    DESCRIPTION:  MONTHLY UNITED MOTOR CLUB INSURANCE EXTRACTION
      *
      * CREATE /USR/DATA/R#/ED/UMC.YYMMDD    HOLDS NEW PREMIUMS
      * RSZ 490
      *
      * READS EACH BRANCHES TRFILE
      *
      * MEMBERSHIP APPLICATIONS SHOULD BE SENT TO UMC@PADUCAH.COM
      *  IN A MICROSOFT EXCEL CSV FILE FORMAT
      *
      * REV:
      *  041029 BAH NEW FOR WORLD
      *  041202 BAH ADDED THE FEECODE TO THE OUTPUT FILE, JIM
      *  050117 BAH CHECK TR1-DAY > END-DD !, JIM
      *  051219 BAH WORLD PR# 445 THEY WILL ONLY SELL ONE YEAR OR GREATER
      *             POLICIES OR A MONTHLY POLICY, NO MORE 6 MONTH OPTION
      *             EXCEPT FOR THE "GA" LITTLE CARD
      *  060628 BAH NEW FORM, NEW ANNUAL RATES, "GA" CONFORMING TO THE REST
      *             OF THE STATES NOW, WORLD PR# 479
      *             60 NOW 72 AND 90 NOW 108
      *  060825 BAH NEW COMPUTATION OF TERM/RATES, WORLD PR# 479 (REVISION)
      *  081114 BAH ALLOW REGMGM TO USE PR# 3283
      *
      * KEC 2016.0318 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      * MJD 171005 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181217 GLOBAL LNFILE
      * BAH 20191003 REMOVED ACCESS-CALL LNFILE, TRL, TRS
      * BAH 20200831 REMOVED RESET ON BR1 
      * BAH 20210413 CHANGE UMC-SSNO TO UMC-CUSTREF AS DATE OF BIRTH AND LAST
      *              4 OF SSNO. MMDDYY9999 #1494
      * BAH 2021.1019 NEW COMPUTATION OF TERM/RATES, WORLD PR# 1520
      * MB  2023.1011 PERFORMANCE TUNING - ADDED STORED PROCEDURES USING
      *               BRANCHLIST PROCESSING. EMBEDDED SQL QUERIES
      *               LN-REC HAS BEEN ADDED TO THE WK FILE, ALONG WITH HARDCODED
      *               SIZE. IF THE SIZE OF LNFILE TABLE INCREASE, THE 78 LEVEL 
      *               ON WK-FILE WILL ALSO NEED TO BE INCREASED. 
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

           SELECT UMC-FILE ASSIGN TO UMC-PATH
                           ORGANIZATION RELATIVE
                           ACCESS DYNAMIC
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           RELATIVE KEY UMC-KEY
                           FILE STATUS FILE-STAT.
      * VALID LOANS FILE
       COPY "LIBGB/GBFSWKI.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      **************************************************
      *  UNITED MOTOR CLUB MEMBERSHIP APPLICATION FILE
      **************************************************
       FD  UMC-FILE
           LABEL RECORDS ARE STANDARD.

       01  UMC-REC.
      * 1 - 6  OFFICE CODE
           03  UMC-COMPANY-CODE            PIC 99.
           03  UMC-BRANCH-NO               PIC 9(4).
      * 7 - 16
           03  UMC-CUSTREF.
               05  UMC-BDATE-MMDDYY        PIC 9(6).
               05  UMC-LAST4-SSNO          PIC 9(4).
      * 17 - 17
           03  FILLER                      PIC X.
      * 18 - 47
           03  UMC-FNAME                   PIC X(30).
      * 48 - 77
           03  UMC-LNAME                   PIC X(30).
      * 78 - 101
           03  UMC-ADR1                    PIC X(24).
      * 102 - 111
           03  FILLER                      PIC X(10).
      * 112 - 135
           03  UMC-ADR2                    PIC X(24).
      * 136 - 177
           03  FILLER                      PIC X(42).
      * 178 - 212
           03  UMC-CITY                    PIC X(35).
      * 213 - 214
           03  UMC-STATE                   PIC X(2).
      * 215 - 224
           03  UMC-ZIPCODE.
               05  UMC-ZIPCODE5                PIC X(5).
               05  UMC-HYPHEN                  PIC X.
               05  UMC-ZIPCODE4                PIC X(4).
      * 225 - 226
           03  UMC-TERM                    PIC 9(2).
      * 227 - 234
           03  UMC-AMOUNT                  PIC 9(4).99-.
      * 235 - 242
           03  UMC-EFFDATE                 PIC 99/99/99.
      * 243 - 272
           03  UMC-COFNAME1                 PIC X(30).
      * 273 - 302
           03  UMC-COLNAME1                PIC X(30).
      * 303 - 332
           03  UMC-COFNAME2                PIC X(30).
      * 333 - 362
           03  UMC-COLNAME2                PIC X(30).
      * 363 - 392
           03  UMC-COFNAME3                PIC X(30).
      * 393 - 422
           03  UMC-COLNAME3                PIC X(30).
      * 423 - 452
           03  UMC-COFNAME4                PIC X(30).
      * 453 - 482
           03  UMC-COLNAME4                PIC X(30).
      * 483 - 488
           03  UMC-ACCOUNT-NO              PIC 9(6).
      * 489 - 490
           03  UMC-FEECODE                 PIC X(2).
      * 491 - 491
           03  UMC-CR                      PIC X.
      * 492 - 492
           03  UMC-LF                      PIC X.

      *************************************
      *   WORK FILE FOR VALID LOANS
      *************************************

       78  WK-LN-SIZE                    VALUE 2800. 

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WORK-OWNBR       PIC 9999.
               05  WORK-ACCTNO      PIC 9(8).
           03  WORK-UMC             PIC S9(5)V99 COMP-3.
           03  WORK-UMC-S-OR-F      PIC X.
           03  WORK-UMC-FEECODE     PIC XX.
           03  WORK-LN-REC          PIC X(WK-LN-SIZE).


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/UMCEXT.CBL S35 03/26/24-09:49:28 >".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK-IFN               PIC X(2)     VALUE "WK".

       01  UMC-KEY              PIC 9(7)     VALUE 0.

       01  UMC-PATH.
           03  UMC-PATH-BASE          PIC X(09).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  UMC-PATH-MACHINE       PIC X(02).
           03  FILLER                 PIC X(08)   VALUE "/ED/UMC.".
           03  UMC-PATH-YYMMDD        PIC 9(6).

       01  CARRIAGE-RETURN      PIC X        VALUE X"0D".
       01  LINE-FEED            PIC X        VALUE X"0A".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  TRL1-CONN-OPEN-SW    PIC X        VALUE "N".
       01  TRS1-CONN-OPEN-SW    PIC X        VALUE "N".

       COPY "LIBGB/GRSSPW.CPY".

       01  SUB                  PIC 9(2).
       01  HOLD-TERM            PIC 9(3)V99.
       01  HOLD-TERMX REDEFINES HOLD-TERM.
           03  HOLD-WHOLE       PIC 9(3).
           03  HOLD-DEC1        PIC 9.
           03  HOLD-DEC2        PIC 9.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".

       01  TOTAL-RECORDS        PIC S9(9)  COMP-3 VALUE 0.
       01  TOTAL-UMC-AMT        PIC S9(9)V99  COMP-3 VALUE 0.
       01  TOTAL-VOID-RECORDS   PIC S9(9)  COMP-3 VALUE 0.
       01  TOTAL-UMC-VOID-AMT   PIC S9(9)V99  COMP-3 VALUE 0.

       01  HOLD-LN-BR           PIC 9(4) VALUE 9999.
       01  VALID-FEE            PIC X.
       01  FEE-CODE             PIC XX.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH          PIC 9999.
           03  END-BRANCH          PIC 9999.
           03  BEG-LC          PIC 99.
           03  END-LC          PIC 99.
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  FEE-CODE1       PIC XX.
           03  FEE-CODE2       PIC XX.
           03  FEE-CODE3       PIC XX.
           03  FEE-CODE4       PIC XX.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  DESC-BUF            PIC X(20) VALUE SPACES.

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(13)    VALUE " ".
           03  FILLER           PIC X(40) VALUE
           " UNITED MOTOR CLUB EXTRACT ".

       01  HEAD3.
           03  FILLER           PIC X(11) VALUE " ".
           03  FILLER           PIC X(20) VALUE " ".
           03  FILLER           PIC X(14) VALUE "FOR PERIOD OF ".
           03  H-BEGDATE        PIC 99/99/99.
           03  FILLER           PIC X(3) VALUE " - ".
           03  H-ENDDATE        PIC 99/99/99.

       01  HEAD4.
           03  FILLER           PIC X(6) VALUE " ".
           03  FILLER           PIC X(14) VALUE "NEW".
           03  FILLER           PIC X(13) VALUE "VOIDS ".

       01  HEAD5.
           03  FILLER           PIC X(2) VALUE " ".
           03  FILLER           PIC X(14) VALUE "MOTOR CLUB    ".
           03  FILLER           PIC X(13) VALUE "MOTOR CLUB   ".


       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(5).
           03  D-QTY1           PIC Z(6)9.
           03  FILLER           PIC X(7).
           03  D-QTY2           PIC Z(6)9.

       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  D-AMT1           PIC Z,ZZZ,ZZ9.99-.
           03  FILLER           PIC X.
           03  D-AMT2           PIC Z,ZZZ,ZZ9.99-.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  RANGE-SEL        PIC X(112).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/UMCEXT_DEF.CPY".
       COPY "LIBSP/UMCEXT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/UMCEXT_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE UMC-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-TRL1-CONNECTION.
           PERFORM CLOSE-TRS1-CONNECTION.
           CLOSE
               UMC-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ************************
       MAIN-MODULE SECTION.
      ************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "UMCEXT" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE 300 TO BRANCHLIST-PGM-PROC-MAX.

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
      *        GO TO PROCESS-REPORT.
              GO TO END-LOAD-BRANCH-TABLE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

      *     GO TO MOVE-BRANCH-PATH-INFO.
           GO TO LOAD-BRANCH-TABLE. 

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
      *        GO TO PROCESS-REPORT.
              GO TO END-LOAD-BRANCH-TABLE. 

       COPY "LIBGB/GRSSP.CPY".
      * LOAD-BRANCH-TABLE.
      * END-LOAD-BRANCH-TABLE.

           IF BRANCHLIST-PROC-BRANCH-CTR = 0
               GO TO PROCESS-REPORT.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
      *     MOVE BR-MACHINE       TO FILPATH-MACHINE.
      *     MOVE BR-DATA-PATH     TO FILPATH-DATA.
           MOVE "B"              TO LN-PATH-OVERRIDE
                                    TRS-PATH-OVERRIDE
                                    TRL-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.

           PERFORM SORT-TRL-FILE.
           PERFORM SORT-TRS-FILE.

       END-OF-GROUP.
      *     PERFORM CLOSE-LN1-FILE.
      *     GO TO NEXT-GROUP-LOC.

           IF BRANCHLIST-PROC-BRANCH-CTR = BRANCHLIST-PGM-PROC-MAX
               INITIALIZE BRANCHLIST-TBL
                          PASS-BRANCHLIST
                          BRANCHLIST-PROC-BRANCH-CTR
                          BRANCHLIST-SUB
               GO TO NEXT-GROUP-LOC.

       PROCESS-REPORT.
           PERFORM CLOSE-WK-FILE.

           PERFORM OPEN-WK-FILE.

           MOVE 0 TO WORK-OWNBR WORK-ACCTNO.
           PERFORM START-WK-FILE.

           PERFORM RUN-REPORT.

           PERFORM PRINT-TOTALS.
           PERFORM RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE UMCEXT-QUEUE-POS TO Q-POS.
           MOVE UMCEXT-COPIES-POS TO C-POS.
           MOVE UMCEXT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".


      * CREATE WORK FILE FOR VALID LOANS
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO INITIALIZE-ENDIT.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "CREATE IN PROGRESS................" TO MESS
              PERFORM SEND-LEGEND.

           MOVE EXT-FILPATH-BASE     TO UMC-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE  TO UMC-PATH-MACHINE.
           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD          TO UMC-PATH-YYMMDD.

           PERFORM OPEN-UMC-FILE-OUTPUT.
           PERFORM CLOSE-UMC-FILE.
           PERFORM OPEN-UMC-FILE.

           PERFORM OPEN-PR-FILE.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE BEG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-BEGDATE.
           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-ENDDATE.

           MOVE 0 TO TOTAL-RECORDS.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
       ENTER-PARAM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.
           IF VDUSTATUS = "1U"
              GO TO FEE-04.
       BR-01.
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
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              PERFORM INVALID-RANGE
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

       LC-01.
           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LC-01.
           MOVE VDUNUM0 TO BEG-LC.
       LC-02.
           MOVE "ENNN020LC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO LC-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LC-02.
           MOVE VDUNUM0 TO END-LC.
           IF BEG-LC > END-LC
              PERFORM INVALID-RANGE
              GO TO LC-01.
           GO TO DATE-01.
       ALL-LC.
           MOVE "SNNN020LC1." TO VDU.
           MOVE 1 TO BEG-LC VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-LC VDUNUM0.
           PERFORM SCREENIO.
       DATE-01.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO FEE-01.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DATE-01.

           IF VDUNUM0 = 0
              GO TO DATE-01
           ELSE
              MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *----------------------------------------------------------------
       DATE-02.
           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DATE-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DATE-02.

           IF VDUNUM0 = 0
              GO TO DATE-02
           ELSE
              MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           GO TO FEE-01.

      *----------------------------------------------------------------
       ALL-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE 1      TO DATE-YYYYMMDD-DD.
           MOVE "S  8080DATE1." TO VDU.
           MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD BEG-DATE NDTE-DATE.
           PERFORM SCREENIO.

           MOVE 31 TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           MOVE "S  8080DATE2." TO VDU.
           MOVE NDTE-DATE TO END-DATE DATE-YYYYMMDD VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       FEE-01.
           MOVE "DESC1." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "E  A020FEE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DATE-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO FEE-01.
           MOVE VDUBUF TO FEE-CODE1 FEE-CODE.

           PERFORM FIND-FEE-DESC.
           IF VALID-FEE NOT = "Y"
              MOVE "INVALID FEE CODE" TO MESS
              PERFORM SEND-MESS
              GO TO FEE-01.

           MOVE DESC-BUF TO WR-BUF.
           MOVE "DESC1." TO WR-LIST.
           PERFORM CALL-WRFORM.

       FEE-02.
           MOVE "DESC2." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "E  A020FEE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FEE-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO FEE-02.
           MOVE VDUBUF TO FEE-CODE2 FEE-CODE.

           PERFORM FIND-FEE-DESC.
           IF VALID-FEE NOT = "Y"
              MOVE "INVALID FEE CODE" TO MESS
              PERFORM SEND-MESS
              GO TO FEE-02.

           MOVE DESC-BUF TO WR-BUF.
           MOVE "DESC2." TO WR-LIST.
           PERFORM CALL-WRFORM.

       FEE-03.
           MOVE "DESC3." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "E  A020FEE3." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FEE-03.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO FEE-03.
           MOVE VDUBUF TO FEE-CODE3 FEE-CODE.

           PERFORM FIND-FEE-DESC.
           IF VALID-FEE NOT = "Y"
              MOVE "INVALID FEE CODE" TO MESS
              PERFORM SEND-MESS
              GO TO FEE-03.

           MOVE DESC-BUF TO WR-BUF.
           MOVE "DESC3." TO WR-LIST.
           PERFORM CALL-WRFORM.

       FEE-04.
           MOVE "DESC4." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "E  A020FEE4." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FEE-03.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO FEE-04.
           MOVE VDUBUF TO FEE-CODE4 FEE-CODE.

           PERFORM FIND-FEE-DESC.
           IF VALID-FEE NOT = "Y"
              MOVE "INVALID FEE CODE" TO MESS
              PERFORM SEND-MESS
              GO TO FEE-04.

           MOVE DESC-BUF TO WR-BUF.
           MOVE "DESC4." TO WR-LIST.
           PERFORM CALL-WRFORM.

       EXIT-PARM.
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-LC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-LC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8080DATE1." TO VDU.
           MOVE BEG-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8080DATE2." TO VDU.
           MOVE END-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A020FEE1." TO VDU.
           MOVE FEE-CODE1 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020FEE2." TO VDU.
           MOVE FEE-CODE2 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020FEE3." TO VDU.
           MOVE FEE-CODE3 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020FEE4." TO VDU.
           MOVE FEE-CODE4 TO VDUBUF.
           PERFORM SCREENIO.

      ***************************
       FIND-FEE-DESC SECTION.
      ***************************

           IF FEE-CODE = SPACES
              MOVE "Y" TO VALID-FEE
              MOVE SPACES TO DESC-BUF
              GO TO EXIT-FIND-FEE.

           MOVE "N"    TO VALID-FEE.

           MOVE "GC"   TO CDB-TYPE.
           MOVE ZEROES TO CDB-BRANCH.
           MOVE SPACES TO CDB-CODE.
           PERFORM OPEN-CDB1-FILE.

           PERFORM START-CDB1-FILE.
       FEE-NEXT-01.
           PERFORM READ-CDB1-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-FIND-FEE.

           IF CDB-TYPE NOT = "GC"
              GO TO EXIT-FIND-FEE.

           IF CDB-CODE NOT = FEE-CODE
              GO TO FEE-NEXT-01.

           MOVE CDB-DESC TO DESC-BUF.
           MOVE "Y" TO VALID-FEE.
       EXIT-FIND-FEE.
           PERFORM CLOSE-CDB1-FILE.

      *************************************************
       RUN-REPORT SECTION.
      *************************************************

       KEY-FILE-LOOP.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-RUN-REPORT.

      * IF A NEW LOAN THEN A VOID WITHIN SAME REPORTING PERIOD,
      * THIS WILL NET TO ZERO
      * NO NEED TO REPORT
           IF WORK-UMC = 0
              GO TO KEY-FILE-LOOP.

           IF WORK-OWNBR NOT = HOLD-LN-BR
              MOVE WORK-OWNBR TO HOLD-LN-BR BR-NO
              PERFORM GET-LN-BW-PATH
              IF EXT-ROUTE-BUF = "00"
                 STRING "CREATE IN PROGRESS............", BR-NO 
                     DELIMITED "  " INTO MESS
                 PERFORM SEND-LEGEND
              END-IF.

      * LNFILE DATA NOW HELD IN WORK FILE 2023-10-11
      *     MOVE WORK-OWNBR  TO LN-OWNBR .
      *     MOVE WORK-ACCTNO TO LN-ACCTNO.
      *     PERFORM READ-LN1-FILE.
      *     IF IO-FG NOT = 0
      *        GO TO END-RUN-REPORT.

           MOVE WORK-LN-REC TO LN-REC.

           PERFORM KEY-DETAIL.

           GO TO KEY-FILE-LOOP.

       END-RUN-REPORT.

      *****************************************
       SORT-TRL-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRL1-FILE.

      *     MOVE BR-NO               TO TL-BRNO
      *                                 QTRL1-WBEG-BRNO
      *                                 QTRL1-WEND-BRNO.

           MOVE BEG-DATE            TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.
           MOVE BEG-LC              TO TL-CLASS
                                       QTRL1-WBEG-CLASS.
           MOVE END-LC              TO QTRL1-WEND-CLASS.

      *     MOVE 0                   TO TL-ACCTNO TL-LAWCODE TL-LINE
      *                                 QTRL1-WBEG-ACCTNO
      *                                 QTRL1-WBEG-LAWCODE
      *                                 QTRL1-WBEG-LINE.
      *     MOVE ALL "9"             TO QTRL1-WEND-ACCTNO
      *                                 QTRL1-WEND-LAWCODE
      *                                 QTRL1-WEND-LINE.

           PERFORM START-TRL1-FILE.   *>  SSP TRL LN

       SORT-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.   *>  EMBEDDED TRL LN
           IF (IO-FG NOT = 0)           *> OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO SORT-TRL-EXIT.

           IF TL-DATE > END-DATE
              GO TO SORT-TRL-EXIT.

           IF TL-CLASS < BEG-LC OR TL-CLASS > END-LC
              GO TO SORT-TRL-NEXT.

           MOVE TL-BRNO   TO WORK-OWNBR.
           MOVE TL-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WORK-UMC.

           PERFORM CHECK-TRL-FEECODES.

           MOVE LN-REC TO WORK-LN-REC.

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRL-NEXT.

       SORT-TRL-EXIT.
           PERFORM CLOSE-TRL1-FILE.

      *****************************************
       SORT-TRS-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRS1-FILE.

      *     MOVE BR-NO               TO TS-BRNO
      *                                 QTRS1-WBEG-BRNO
      *                                 QTRS1-WEND-BRNO.

           MOVE BEG-DATE            TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WEND-DATE.
           MOVE BEG-LC              TO TS-CLASS
                                       QTRS1-WBEG-CLASS.
           MOVE END-LC              TO QTRS1-WEND-CLASS.

      *     MOVE 0                   TO TS-ACCTNO TS-LAWCODE TS-LINE
      *                                 QTRS1-WBEG-ACCTNO
      *                                 QTRS1-WBEG-LAWCODE
      *                                 QTRS1-WBEG-LINE.
      *     MOVE ALL "9"             TO QTRS1-WEND-ACCTNO
      *                                 QTRS1-WEND-LAWCODE
      *                                 QTRS1-WEND-LINE.

           PERFORM START-TRS1-FILE.  *> SSP TRS LN

       SORT-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.  *> EMBEDDED TRS LN
           IF (IO-FG NOT = 0) OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO SORT-TRS-EXIT.

           IF TS-DATE > END-DATE
              GO TO SORT-TRS-EXIT.

           IF TS-CLASS < BEG-LC OR TS-CLASS > END-LC
              GO TO SORT-TRS-NEXT.

           MOVE TS-BRNO   TO WORK-OWNBR.
           MOVE TS-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WORK-UMC.

           PERFORM CHECK-TRS-FEECODES.

           MOVE LN-REC TO WORK-LN-REC.

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRS-NEXT.

       SORT-TRS-EXIT.
           PERFORM CLOSE-TRS1-FILE.

      ******************************
       CHECK-TRL-FEECODES SECTION.
      ******************************
           MOVE 0 TO SUB.
       CHECK-TRL-NEXT.
           ADD 1 TO SUB.
           IF SUB > 10
              GO TO EXIT-FIND-TRL.

           IF TL-FEECODE(SUB) = SPACES
              GO TO CHECK-TRL-NEXT.

           IF TL-FEECODE(SUB) = FEE-CODE1 OR
               TL-FEECODE(SUB) = FEE-CODE2 OR
                TL-FEECODE(SUB) = FEE-CODE3 OR
                 TL-FEECODE(SUB) = FEE-CODE4
               COMPUTE WORK-UMC = WORK-UMC + TL-FEEAMT(SUB)
               MOVE TL-FEECODE(SUB) TO WORK-UMC-FEECODE
               IF TL-FEECODE(SUB) = BR-ROAD-G-SGL-FEECODE
                  MOVE "S" TO WORK-UMC-S-OR-F
               ELSE
               IF TL-FEECODE(SUB) = BR-ROAD-G-FAM-FEECODE
                  MOVE "F" TO WORK-UMC-S-OR-F.

           GO TO CHECK-TRL-NEXT.

       EXIT-FIND-TRL.
           EXIT.

      ******************************
       CHECK-TRS-FEECODES SECTION.
      ******************************
           MOVE 0 TO SUB.
       CHECK-TRS-NEXT.
           ADD 1 TO SUB.
           IF SUB > 10
              GO TO EXIT-FIND-TRS.

           IF TS-FEECODE(SUB) = SPACES
              GO TO CHECK-TRS-NEXT.

           IF TS-FEECODE(SUB) = FEE-CODE1 OR
               TS-FEECODE(SUB) = FEE-CODE2 OR
                TS-FEECODE(SUB) = FEE-CODE3 OR
                 TS-FEECODE(SUB) = FEE-CODE4
               MOVE TS-FEECODE(SUB) TO WORK-UMC-FEECODE
               COMPUTE WORK-UMC = WORK-UMC + TS-FEEAMT(SUB)
               IF TS-FEECODE(SUB) = BR-ROAD-G-SGL-FEECODE
                  MOVE "S" TO WORK-UMC-S-OR-F
               ELSE
               IF TS-FEECODE(SUB) = BR-ROAD-G-FAM-FEECODE
                  MOVE "F" TO WORK-UMC-S-OR-F.

           GO TO CHECK-TRS-NEXT.

       EXIT-FIND-TRS.
           EXIT.

      ****************************
       KEY-DETAIL SECTION.
      ****************************

           MOVE SPACES TO UMC-REC.

           MOVE 20 TO UMC-COMPANY-CODE.
           MOVE BR-NO TO UMC-BRANCH-NO.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID" TO BW-LNAME BW-FNAME
                                BW-CITY BW-ZIP BW-BDATE.

           MOVE BW-BDATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO UMC-BDATE-MMDDYY.
           MOVE LN-SSNO(1)  TO UMC-LAST4-SSNO.

           MOVE BW-FNAME TO UMC-FNAME.
           MOVE BW-LNAME TO UMC-LNAME.
           MOVE BW-ADR1 TO UMC-ADR1.
           MOVE BW-ADR2 TO UMC-ADR2.
           MOVE BW-CITY TO UMC-CITY.
           MOVE BW-STATE TO UMC-STATE.
           MOVE BW-ZIP TO UMC-ZIPCODE.
      * JUST IN CASE ITS NOT THERE....
           MOVE "-" TO UMC-HYPHEN.

           PERFORM FIND-WORLD-TERM.

           MOVE WORK-UMC TO UMC-AMOUNT.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO UMC-EFFDATE.

           IF WORK-UMC-S-OR-F = "F"
              IF LN-SSNO(2) NOT = 0
                 MOVE LN-SSNO(2) TO BW-SSNO
                 PERFORM READ-BW1-FILE
                 IF IO-FG = 0
                    MOVE BW-FNAME TO UMC-COFNAME1
                    MOVE BW-LNAME TO UMC-COLNAME1.
           IF WORK-UMC-S-OR-F = "F"
              IF LN-SSNO(3) NOT = 0
                 MOVE LN-SSNO(3) TO BW-SSNO
                 PERFORM READ-BW1-FILE
                 IF IO-FG = 0
                    MOVE BW-FNAME TO UMC-COFNAME2
                    MOVE BW-LNAME TO UMC-COLNAME2.
           IF WORK-UMC-S-OR-F = "F"
              IF LN-SSNO(4) NOT = 0
                 MOVE LN-SSNO(4) TO BW-SSNO
                 PERFORM READ-BW1-FILE
                 IF IO-FG = 0
                    MOVE BW-FNAME TO UMC-COFNAME3
                    MOVE BW-LNAME TO UMC-COLNAME3.

           MOVE SPACES TO UMC-COFNAME4 UMC-COLNAME4.
           MOVE LN-ACCTNO TO UMC-ACCOUNT-NO.
           MOVE WORK-UMC-FEECODE TO UMC-FEECODE.

           MOVE CARRIAGE-RETURN TO UMC-CR.
           MOVE LINE-FEED TO UMC-LF.

           ADD 1 TO UMC-KEY.
           PERFORM WRITE-UMC-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.


           IF WORK-UMC > 0
              ADD WORK-UMC TO TOTAL-UMC-AMT
              ADD 1 TO TOTAL-RECORDS
           ELSE
              ADD WORK-UMC TO TOTAL-UMC-VOID-AMT
              ADD 1 TO TOTAL-VOID-RECORDS.

       EXIT-KEY-DETAIL.
           EXIT.


      *****************************
       FIND-WORLD-TERM SECTION.
      *****************************

      * IF 93 (SINGLE) THE TERM SHOULD BE DIVIDED BY 8
      * IF 94 (FAMILY) THE TERM SHOULD BE DIVIDED BY 10
      * ROUND REMAINDER

           IF (WORK-UMC-S-OR-F = "S")
              COMPUTE HOLD-TERM = WORK-UMC / 8
           ELSE
              COMPUTE HOLD-TERM = WORK-UMC / 10.

      * IF NO REMAINDER, THEN A GOOD TERM HAS BEEN FOUND
           IF (HOLD-DEC1 = 0 AND HOLD-DEC2 = 0)
              MOVE HOLD-TERM TO UMC-TERM
           ELSE
      * IF REMAINDER, MUST BUMP UP A MONTH
                 ADD 1 TO HOLD-TERM
                 MOVE HOLD-TERM TO UMC-TERM.

       FIND-WORLD-TERM-EXIT.
           EXIT.

      ***************************
       PRINT-TOTALS SECTION.
      ***************************

           MOVE HEAD4 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD5 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE TOTAL-RECORDS TO D-QTY1.
           MOVE TOTAL-VOID-RECORDS TO D-QTY2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE TOTAL-UMC-AMT TO D-AMT1.
           MOVE TOTAL-UMC-VOID-AMT TO D-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      ********************
       RANGE-LINES SECTION.
      ********************
           MOVE 3 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-LC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END LOAN CLASS" TO RANGE-LINE.
           MOVE END-LC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DATE" TO RANGE-LINE.
           MOVE BEG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    DATE" TO RANGE-LINE.
           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "FEE CODES " TO RANGE-LINE.
           MOVE FEE-CODE1 TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE FEE-CODE2 TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE FEE-CODE3 TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE FEE-CODE4 TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

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
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO UMCEXT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO UMCEXT-SYSDATE.
           DISPLAY UMCEXT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE UMCEXT-FORM-FIELDS TO WR-BUF.
           MOVE UMCEXT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE UMCEXT-HELP--FILE TO HELP-LINK-FILE.
           MOVE UMCEXT-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/UMCEXT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-LN-BW-PATH.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.

           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              BW-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

      ************************************************
       IO-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       OPEN-UMC-FILE.
           PERFORM OPEN-IT.
           MOVE "UMC" TO E-FILE.
           OPEN I-O UMC-FILE.
           IF IO-FG = 8
              GO TO OPEN-UMC-FILE.
           IF IO-FG = 7
              CLOSE UMC-FILE
              GO TO OPEN-UMC-FILE.
       OPEN-UMC-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE "UMC" TO E-FILE.
           OPEN OUTPUT UMC-FILE.
           IF IO-FG = 8
              GO TO OPEN-UMC-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE UMC-FILE
              GO TO OPEN-UMC-FILE-OUTPUT.
       WRITE-UMC-FILE.
           PERFORM WRITE-IT.
           MOVE "UMC" TO E-FILE.
           MOVE UMC-KEY TO E-KEYX.
           WRITE UMC-REC.
           IF IO-FG = 8
              GO TO WRITE-UMC-FILE.
       CLOSE-UMC-FILE.
           PERFORM CLOSE-IT.
           CLOSE UMC-FILE.

       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRS-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRS-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH.

      *-----------------------------------------------------------------
       OPEN-TRS1-FILE.
           PERFORM LOAD-TRS1-FILE.
           PERFORM OPEN-IT.
           MOVE TRS-PATH-SQL TO E-FILE.

           IF TRS1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRS1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO TRS1-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRS1-FILE.   *> SSP TRS LN
           PERFORM START-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.

           IF ( TRS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRS1-FILE.

           EXEC SQL SET CONNECTION TRS1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRS1_CURSOR CURSOR FOR
                :TRS1-RETURN-CODE = 
                 EXEC SSP_UMCEXT_TRSFILE_SELECT
                  ( :PASS-BRANCHLIST,
                    :QTRS1-WBEG-DATE,  :QTRS1-WEND-DATE,
                    :QTRS1-WBEG-CLASS, :QTRS1-WEND-CLASS) 
                END-EXEC.

      *     SELECT TRSFILE.TS_BRNO,
      *            CAST(TRSFILE.TS_DATE AS VARCHAR(10)),
      *            TRSFILE.TS_CLASS,
      *            TRSFILE.TS_ACCTNO,
      *            TRSFILE.TS_LAWCODE,
      *            TRSFILE.TS_LINE,
      *            TRSFILE.TS_FEECODE_1,
      *            TRSFILE.TS_FEECODE_2,
      *            TRSFILE.TS_FEECODE_3,
      *            TRSFILE.TS_FEECODE_4,
      *            TRSFILE.TS_FEECODE_5,
      *            TRSFILE.TS_FEECODE_6,
      *            TRSFILE.TS_FEECODE_7,
      *            TRSFILE.TS_FEECODE_8,
      *            TRSFILE.TS_FEECODE_9,
      *            TRSFILE.TS_FEECODE_10,
      *            TRSFILE.TS_FEECODE_11,
      *            TRSFILE.TS_FEECODE_12,
      *            TRSFILE.TS_FEECODE_13,
      *            TRSFILE.TS_FEECODE_14,
      *            TRSFILE.TS_FEECODE_15,
      *            TRSFILE.TS_FEECODE_16,
      *            TRSFILE.TS_FEECODE_17,
      *            TRSFILE.TS_FEECODE_18,
      *            TRSFILE.TS_FEECODE_19,
      *            TRSFILE.TS_FEECODE_20,
      *            TRSFILE.TS_FEEAMT_1,
      *            TRSFILE.TS_FEEAMT_2,
      *            TRSFILE.TS_FEEAMT_3,
      *            TRSFILE.TS_FEEAMT_4,
      *            TRSFILE.TS_FEEAMT_5,
      *            TRSFILE.TS_FEEAMT_6,
      *            TRSFILE.TS_FEEAMT_7,
      *            TRSFILE.TS_FEEAMT_8,
      *            TRSFILE.TS_FEEAMT_9,
      *            TRSFILE.TS_FEEAMT_10,
      *            TRSFILE.TS_FEEAMT_11,
      *            TRSFILE.TS_FEEAMT_12,
      *            TRSFILE.TS_FEEAMT_13,
      *            TRSFILE.TS_FEEAMT_14,
      *            TRSFILE.TS_FEEAMT_15,
      *            TRSFILE.TS_FEEAMT_16,
      *            TRSFILE.TS_FEEAMT_17,
      *            TRSFILE.TS_FEEAMT_18,
      *            TRSFILE.TS_FEEAMT_19,
      *            TRSFILE.TS_FEEAMT_20,
      *            LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_SSNO1,
      *            LNFILE.LN_MAKERCD1, 
      *            LNFILE.LN_SSNO2,
      *            LNFILE.LN_MAKERCD2, 
      *            LNFILE.LN_SSNO3,
      *            LNFILE.LN_MAKERCD3, 
      *            LNFILE.LN_SSNO4,
      *            LNFILE.LN_MAKERCD4, 
      *            CAST(LNFILE.LN_LOANDATE AS VARCHAR(10))
      *         FROM #TEMP1_BRANCH_GROUP_TBL T1
      *         INNER JOIN DBO.TRSFILE TRSFILE
      *          ON TRSFILE.TS_BRNO = T1.BRNO
      *         INNER JOIN DBO.LNFILE LNFILE
      *          ON LNFILE.LN_OWNBR  = TRSFILE.TS_BRNO
      *          AND LNFILE.LN_ACCTNO = TRSFILE.TS_ACCTNO
      *        WHERE TRSFILE.TS_DATE  BETWEEN @QTRS1_WBEG_DATE  AND @QTRS1_WEND_DATE
      *          AND TRSFILE.TS_CLASS BETWEEN @QTRS1_WBEG_CLASS AND @QTRS1_WEND_CLASS
      *        ORDER BY T1.SEQ, TRSFILE.TS_DATE,
      *                TRSFILE.TS_CLASS,
      *                TRSFILE.TS_ACCTNO,
      *                TRSFILE.TS_LINE

           EXEC SQL
               OPEN TRS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRS1-WBEG-CLASS
                          QTRS1-WEND-CLASS.
           MOVE SPACES TO QTRS1-WBEG-DATE
                          QTRS1-WEND-DATE.

           MOVE STAT---GOOD TO TRS1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRS1-FILE-NEXT.   *> EMBEDDED TRS LN
           PERFORM READ-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           INITIALIZE QTRS-REC QLN-REC.

           EXEC SQL SET CONNECTION TRS1_CONNECT END-EXEC.

           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH TRS1_CURSOR INTO 
                   :QTS-BRNO,
                   :QTS-DATE,
                   :QTS-CLASS,
                   :QTS-ACCTNO,
                   :QTS-LAWCODE,
                   :QTS-LINE,
                   :QTS-FEECODE-1,
                   :QTS-FEECODE-2,
                   :QTS-FEECODE-3,
                   :QTS-FEECODE-4,
                   :QTS-FEECODE-5,
                   :QTS-FEECODE-6,
                   :QTS-FEECODE-7,
                   :QTS-FEECODE-8,
                   :QTS-FEECODE-9,
                   :QTS-FEECODE-10,
                   :QTS-FEECODE-11,
                   :QTS-FEECODE-12,
                   :QTS-FEECODE-13,
                   :QTS-FEECODE-14,
                   :QTS-FEECODE-15,
                   :QTS-FEECODE-16,
                   :QTS-FEECODE-17,
                   :QTS-FEECODE-18,
                   :QTS-FEECODE-19,
                   :QTS-FEECODE-20,
                   :QTS-FEEAMT-1,
                   :QTS-FEEAMT-2,
                   :QTS-FEEAMT-3,
                   :QTS-FEEAMT-4,
                   :QTS-FEEAMT-5,
                   :QTS-FEEAMT-6,
                   :QTS-FEEAMT-7,
                   :QTS-FEEAMT-8,
                   :QTS-FEEAMT-9,
                   :QTS-FEEAMT-10,
                   :QTS-FEEAMT-11,
                   :QTS-FEEAMT-12,
                   :QTS-FEEAMT-13,
                   :QTS-FEEAMT-14,
                   :QTS-FEEAMT-15,
                   :QTS-FEEAMT-16,
                   :QTS-FEEAMT-17,
                   :QTS-FEEAMT-18,
                   :QTS-FEEAMT-19,
                   :QTS-FEEAMT-20,     
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-SSNO1,
                   :QLN-MAKERCD1, 
                   :QLN-SSNO2,
                   :QLN-MAKERCD2, 
                   :QLN-SSNO3,
                   :QLN-MAKERCD3, 
                   :QLN-SSNO4,
                   :QLN-MAKERCD4, 
                   :QLN-LOANDATE 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRS-FIELDS
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRS1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TRS1_CONNECT END-EXEC
              EXEC SQL
                  CLOSE TRS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRS1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRS1-CONNECTION.

           IF TRS1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION TRS1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT TRS1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO TRS1-CONN-OPEN-SW.
      *-----------------------------------------------------------------


      * COPY "LIBLP/LPTRL1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRL-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRL-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH.

      *-----------------------------------------------------------------
       OPEN-TRL1-FILE.
           PERFORM LOAD-TRL1-FILE.
           PERFORM OPEN-IT.
           MOVE TRL-PATH-SQL TO E-FILE.

           IF TRL1-CONN-OPEN-SW = "N"
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRL1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO TRL1-CONN-OPEN-SW.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRL1-FILE.   *>  SSP TRL LN
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           IF ( TRL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRL1-FILE.

      *  SELECT TRLFILE.TL_BRNO,
      *            CAST(TRLFILE.TL_DATE AS VARCHAR(10)),
      *            TRLFILE.TL_CLASS,
      *            TRLFILE.TL_ACCTNO,
      *            TRLFILE.TL_LAWCODE,
      *            TRLFILE.TL_LINE,
      *            TRLFILE.TL_FEECODE_1,
      *            TRLFILE.TL_FEECODE_2,
      *            TRLFILE.TL_FEECODE_3,
      *            TRLFILE.TL_FEECODE_4,
      *            TRLFILE.TL_FEECODE_5,
      *            TRLFILE.TL_FEECODE_6,
      *            TRLFILE.TL_FEECODE_7,
      *            TRLFILE.TL_FEECODE_8,
      *            TRLFILE.TL_FEECODE_9,
      *            TRLFILE.TL_FEECODE_10,
      *            TRLFILE.TL_FEECODE_11,
      *            TRLFILE.TL_FEECODE_12,
      *            TRLFILE.TL_FEECODE_13,
      *            TRLFILE.TL_FEECODE_14,
      *            TRLFILE.TL_FEECODE_15,
      *            TRLFILE.TL_FEECODE_16,
      *            TRLFILE.TL_FEECODE_17,
      *            TRLFILE.TL_FEECODE_18,
      *            TRLFILE.TL_FEECODE_19,
      *            TRLFILE.TL_FEECODE_20,
      *            TRLFILE.TL_FEEAMT_1,
      *            TRLFILE.TL_FEEAMT_2,
      *            TRLFILE.TL_FEEAMT_3,
      *            TRLFILE.TL_FEEAMT_4,
      *            TRLFILE.TL_FEEAMT_5,
      *            TRLFILE.TL_FEEAMT_6,
      *            TRLFILE.TL_FEEAMT_7,
      *            TRLFILE.TL_FEEAMT_8,
      *            TRLFILE.TL_FEEAMT_9,
      *            TRLFILE.TL_FEEAMT_10,
      *            TRLFILE.TL_FEEAMT_11,
      *            TRLFILE.TL_FEEAMT_12,
      *            TRLFILE.TL_FEEAMT_13,
      *            TRLFILE.TL_FEEAMT_14,
      *            TRLFILE.TL_FEEAMT_15,
      *            TRLFILE.TL_FEEAMT_16,
      *            TRLFILE.TL_FEEAMT_17,
      *            TRLFILE.TL_FEEAMT_18,
      *            TRLFILE.TL_FEEAMT_19,
      *            TRLFILE.TL_FEEAMT_20,
      *            LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_SSNO1,
      *            LNFILE.LN_MAKERCD1, 
      *            LNFILE.LN_SSNO2,
      *            LNFILE.LN_MAKERCD2, 
      *            LNFILE.LN_SSNO3,
      *            LNFILE.LN_MAKERCD3, 
      *            LNFILE.LN_SSNO4,
      *            LNFILE.LN_MAKERCD4, 
      *            CAST(LNFILE.LN_LOANDATE AS VARCHAR(10))
      *    FROM #TEMP1_BRANCH_GROUP_TBL T1
      *    INNER JOIN DBO.TRLFILE TRLFILE
      *      ON TRLFILE.TL_BRNO = T1.BRNO
      *     FROM  DBO.TRLFILE TRLFILE
      *    INNER JOIN DBO.LNFILE LNFILE
      *     ON LNFILE.LN_OWNBR  = TRLFILE.TL_BRNO
      *     AND LNFILE.LN_ACCTNO = TRLFILE.TL_ACCTNO
      *    WHERE TRLFILE.TL_DATE  BETWEEN @QTRL1_WBEG_DATE  AND @QTRL1_WEND_DATE
      *     AND  TRLFILE.TL_CLASS BETWEEN @QTRL1_WBEG_CLASS AND @QTRL1_WEND_CLASS
      *    ORDER BY T1.SEQ, TRLFILE.TL_DATE,
      *         TRLFILE.TL_CLASS,
      *         TRLFILE.TL_ACCTNO,
      *         TRLFILE.TL_LAWCODE,
      *         TRLFILE.TL_LINE

           EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRL1_CURSOR CURSOR FOR
                :TRL1-RETURN-CODE = EXEC SSP_UMCEXT_TRLFILE_SELECT
              (:PASS-BRANCHLIST,  :QTRL1-WBEG-DATE,  :QTRL1-WEND-DATE,
               :QTRL1-WBEG-CLASS, :QTRL1-WEND-CLASS)

           END-EXEC.

           EXEC SQL
               OPEN TRL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRL1-WBEG-CLASS
                          QTRL1-WEND-CLASS.
           MOVE SPACES TO QTRL1-WBEG-DATE
                          QTRL1-WEND-DATE.

           MOVE STAT---GOOD TO TRL1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRL1-FILE-NEXT.   *>  EMBEDDED TRL LN
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC QLN-REC.

           EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
               FETCH TRL1_CURSOR INTO 
                   :QTL-BRNO,
                   :QTL-DATE ,
                   :QTL-CLASS,
                   :QTL-ACCTNO,
                   :QTL-LAWCODE,
                   :QTL-LINE,
                   :QTL-FEECODE-1,
                   :QTL-FEECODE-2,
                   :QTL-FEECODE-3,
                   :QTL-FEECODE-4,
                   :QTL-FEECODE-5,
                   :QTL-FEECODE-6,
                   :QTL-FEECODE-7,
                   :QTL-FEECODE-8,
                   :QTL-FEECODE-9,
                   :QTL-FEECODE-10,
                   :QTL-FEECODE-11,
                   :QTL-FEECODE-12,
                   :QTL-FEECODE-13,
                   :QTL-FEECODE-14,
                   :QTL-FEECODE-15,
                   :QTL-FEECODE-16,
                   :QTL-FEECODE-17,
                   :QTL-FEECODE-18,
                   :QTL-FEECODE-19,
                   :QTL-FEECODE-20,
                   :QTL-FEEAMT-1,
                   :QTL-FEEAMT-2,
                   :QTL-FEEAMT-3,
                   :QTL-FEEAMT-4,
                   :QTL-FEEAMT-5,
                   :QTL-FEEAMT-6,
                   :QTL-FEEAMT-7,
                   :QTL-FEEAMT-8,
                   :QTL-FEEAMT-9,
                   :QTL-FEEAMT-10,
                   :QTL-FEEAMT-11,
                   :QTL-FEEAMT-12,
                   :QTL-FEEAMT-13,
                   :QTL-FEEAMT-14,
                   :QTL-FEEAMT-15,
                   :QTL-FEEAMT-16,
                   :QTL-FEEAMT-17,
                   :QTL-FEEAMT-18,
                   :QTL-FEEAMT-19,
                   :QTL-FEEAMT-20, 
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-SSNO1,
                   :QLN-MAKERCD1, 
                   :QLN-SSNO2,
                   :QLN-MAKERCD2, 
                   :QLN-SSNO3,
                   :QLN-MAKERCD3, 
                   :QLN-SSNO4,
                   :QLN-MAKERCD4, 
                   :QLN-LOANDATE 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRL1-FILE.
           PERFORM CLOSE-IT.
           IF ( TRL1-CURSOR-STAT-GOOD )
               EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC
               EXEC SQL CLOSE TRL1_CURSOR END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE STAT---BAD TO TRL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-TRL1-CONNECTION.
           IF TRL1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT TRL1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO TRL1-CONN-OPEN-SW.
      *-----------------------------------------------------------------


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ************************************************
       END-PROGRAM SECTION.
      ************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/INSRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY UMCEXT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
