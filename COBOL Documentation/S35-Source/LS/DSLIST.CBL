       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       DSLIST.
       DATE-WRITTEN. 02/10/1999.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  DEALER STATISTIC FILE LIST
      *
      * REV:
      *  MJD 091595 ADDED DS-DATE LOGIC.
      *  JFF 101695 ADDED CLASS SCAN AND DEALER SCAN
      *  KEC 110195 FIXED TO WORK CORRECTLY WITH DS-DATE.
      *  KEC 032197 REVISED FOR NEW FILE LAYOUT OF DS-FILE(APPLICATIONS)
      *               ADDED   NEW APPLICATION STATISTICS FIELDS;
      *               REMOVED OLD APPLICATION STATISTICS FIELDS.
      *  MJD 071197 ADDED DLDISC VOLUME (MERC REQ#160)
      *  MJD 111497 ADDED PRINT OF DELINQ INFO. CHANGED THE WAY RANGES
      *             WERE CHECKED. IF SEQ 1 WAS SELECTED I ALWAYS GOT
      *             "NO RECORDS FOUND...". ADDED START OF DSFILE.
      *  MJD 051998 REMOVED DS-PASTDUE.
      *  JTG 063098 EXPANDED DS-APP-TYPE-TBL TO 3 LEVELS
      *  MJD 990908 ADDED BATCH,  ETC LOGIC (MULL PR# 949)
      *  MJD 000322 REMOVED DS-CNEARN/EARNTAKN/EARNDUE/CNUNEARN/ERNOTTAK
      *             (MERC #223)
      *  MJD 000329 ADDED DS-UNEARNED-AMT AND DS-UNEARNED-AMT2 (MERC PR#223)
      *  MJD 000331 ADDED DS-PROCEEDS (MERC PR#223)
      *  AMM 070212 UPDATED LISTING
      *  BAH 070404 DLFILE NEEDED ALL-PATH, SUPPORT PL# 585
      *  BLM 160513 FIX FOR NEW DS-DATE FORMAT, FIX DS-UNEARNED, PD0003
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      * BAH 181011 GLOBAL DLFILE
      * BAH 181015 GLOBAL DSFILE, ALSO ENTER BRANCH RANGE FOR SEQ 2
      *  CS 190429 REMOVE DS APP STATS AND HEADINGS
      *             SAVED DSLIST.190429
      * BAH 20190930 REMOVED ACCESS-CALL DSFILE
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
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/DSLIST.CBL S35 02/21/24-12:05:29 >".

      ******************************************************************
      *
      *     F I L E   I F N
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".

       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK-IFN               PIC X(2)     VALUE "WK".

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-FEED          PIC 999      VALUE 0.
       01  LN-COUNT         PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      ******************************************************************
      *
      *   MISC WORKERS
      *
      ******************************************************************
       01  SUB                  PIC 9(2)  COMP-3.

      *    -------------------------------------------------------------
      *    THERE IS NO GROUP LOGIC HERE, BUT I NEED THESE FIELDS
      *    FOR BRSECURE
      *    -------------------------------------------------------------
       01  GROUP-FLAG           PIC X(01) VALUE "N".
       01  ENT-GROUP            PIC X(04) VALUE SPACES.

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".

       01  FIRST-TIME           PIC X        VALUE "Y".

       01  PCNTR                PIC 9.
       01  SCAN-ERR             PIC 9(01)    VALUE 01.
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  DAYS-AREA.
           03  FILLER      PIC X(10)    VALUE
               " 30 DAYS: ".
           03  FILLER      PIC X(10)    VALUE
               " 60 DAYS: ".
           03  FILLER      PIC X(10)    VALUE
               "  90-120: ".
           03  FILLER      PIC X(10)    VALUE
               "    150+: ".
           03  FILLER      PIC X(10)    VALUE
               "     <M>: ".
           03  FILLER      PIC X(10)    VALUE
               "     <P>: ".
           03  FILLER      PIC X(10)    VALUE
               " CURRENT: ".
       01  DAYS-TBL REDEFINES DAYS-AREA.
           03  DAYS-ITEM   PIC X(10)    OCCURS 7 TIMES.


       01  TAG-AREA.
           03  FILLER      PIC X(20)    VALUE "OUTSTANDING        :".
           03  FILLER      PIC X(20)    VALUE "JUDGEMENTS         :".
           03  FILLER      PIC X(20)    VALUE "CHARGED OFF (P & L):".
           03  FILLER      PIC X(20)    VALUE "RECOVERED   (P & L):".
           03  FILLER      PIC X(20)    VALUE "NEW PURCHASE       :".
           03  FILLER      PIC X(20)    VALUE "OTHER PURCHASE     :".
           03  FILLER      PIC X(20)    VALUE "CONVERTED          :".
           03  FILLER      PIC X(20)    VALUE "PAID OFF           :".
           03  FILLER      PIC X(20)    VALUE "SERVICE ACCOUNTS   :".
       01  TAG-TBL REDEFINES TAG-AREA.
           03  TAG-ITEM    PIC X(20)    OCCURS 9 TIMES.

       01  HOLD-BEG-KEY.
           03  HOLD-BEG-DATE-CCYYMM  PIC 9(6).
           03  HOLD-BEG-BRANCH       PIC 9(4).
           03  HOLD-BEG-NUM          PIC 9(6).
           03  HOLD-BEG-CLASS        PIC 9(3).

       01  CLASS-FG          PIC X(3)    VALUE "   ".
           88  CLASS-BEG                 VALUE "BEG".
           88  CLASS-END                 VALUE "END".
       01  DEALER-FG         PIC X(4)    VALUE "    ".
           88  DEALER-BEG                VALUE "DBEG".
           88  DEALER-END                VALUE "DEND".


      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
       
           03  SEQ             PIC 9(01).
           03  BEG-BRANCH      PIC 9(04).
           03  END-BRANCH      PIC 9(04).
           03  BEG-DLNUM       PIC 9(06).
           03  END-DLNUM       PIC 9(06).
           03  BEG-CLASS       PIC 9(02).
           03  END-CLASS       PIC 9(02).
           03  DISPLAY-DATE    PIC 9(04).
           03  DISPLAY-DATEX REDEFINES DISPLAY-DATE.
               05  DISPLAY-MO  PIC 99.
               05  DISPLAY-YR  PIC 99.

       01  VERSION-CONTROL     PIC X VALUE "A".

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
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           "- - - DEALER STATISTIC FILE LISTING - - -".
           03  FILLER           PIC X(45)    VALUE " ".

       01  HEAD4.
           03  FILLER      PIC X(44)    VALUE
               "- - - - - - - - - - - - - -  B A L A N C E S".
           03  FILLER      PIC X(31)    VALUE
               "  - - - - - - - - - - - - - - -".
           03  FILLER      PIC X(9).
           03  FILLER      PIC X(41)    VALUE
               "- - -  D L D I S C   V O L U M E S  - - -".

       01  HEAD5.
           03  FILLER      PIC X(5)     VALUE " ".
           03  FILLER      PIC X(7)     VALUE "RESERVE".
           03  FILLER      PIC XXXX     VALUE " ".
           03  FILLER      PIC X(11)    VALUE "PAR RESERVE".
           03  FILLER      PIC XXX      VALUE " ".
           03  FILLER      PIC X(11)    VALUE "SERVICE CHG".
           03  FILLER      PIC X(17)    VALUE " ".
           03  FILLER      PIC X(15)    VALUE "     SUM AMOUNT".
           03  FILLER      PIC X(16)    VALUE " ".
           03  FILLER      PIC X(3)     VALUE "MTD".
           03  FILLER      PIC X(12)    VALUE " ".
           03  FILLER      PIC X(3)     VALUE "YTD".
           03  FILLER      PIC X(12)    VALUE " ".
           03  FILLER      PIC X(3)     VALUE "LYR".

       01  HEAD6.
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(10)    VALUE "SP RESERVE".
           03  FILLER      PIC X(6)     VALUE " ".
           03  FILLER      PIC X(9)     VALUE "COLLECTED".
           03  FILLER      PIC XXX      VALUE " ".
           03  FILLER      PIC X(12)    VALUE "UNPAID CHECK".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(11)    VALUE "  SUM TERMS".
           03  FILLER      PIC X(5)     VALUE " ".
           03  FILLER      PIC X(13)    VALUE "   SUM  TOTAL".

       01  HEAD7.
           03  FILLER      PIC X(45)    VALUE
               "- - O P E N  I N S U R A N C E  A C C T S - -".
           03  FILLER      PIC X(10)    VALUE " ".
           03  FILLER      PIC X(52)    VALUE
               "- - - - - - - R E C O U R S E   O U T S T A N D I N ".
           03  FILLER      PIC X(17)    VALUE "G - - - - - - - -".

       01  HEAD8.
           03  FILLER      PIC X(6)     VALUE "W/CRED".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(7)     VALUE "W/CRED ".
           03  FILLER      PIC X(7)     VALUE "W/ACC  ".
           03  FILLER      PIC X(7)     VALUE "W/PP   ".
           03  FILLER      PIC X(7)     VALUE "W/O1   ".
           03  FILLER      PIC X(7)     VALUE "W/O2   ".
           03  FILLER      PIC X(7)     VALUE "W/NO".
           03  FILLER      PIC X(6)     VALUE " ".
           03  FILLER      PIC XXXX     VALUE "FULL".
           03  FILLER      PIC X(10)    VALUE " ".
           03  FILLER      PIC XXXX     VALUE "FULL".
           03  FILLER      PIC X(5)     VALUE " ".
           03  FILLER      PIC X(7)     VALUE "PARTIAL".
           03  FILLER      PIC X(6)     VALUE " ".
           03  FILLER      PIC X(7)     VALUE "PARTIAL".
           03  FILLER      PIC X(6)     VALUE " ".
           03  FILLER      PIC XXX      VALUE "NON".
           03  FILLER      PIC X(10)    VALUE " ".
           03  FILLER      PIC XXX      VALUE "NON".

       01  HEAD9.
           03  FILLER      PIC X(8)     VALUE "LIFE 1  ".
           03  FILLER      PIC X(7)     VALUE "LIFE 2 ".
           03  FILLER      PIC X(9)     VALUE "HEALTH ".
           03  FILLER      PIC XXXX     VALUE "INS.".
           03  FILLER      PIC XXX      VALUE " ".
           03  FILLER      PIC XXXX     VALUE "INS.".
           03  FILLER      PIC XXX      VALUE " ".
           03  FILLER      PIC XXXX     VALUE "INS.".
           03  FILLER      PIC XXX      VALUE " ".
           03  FILLER      PIC XXXX     VALUE "INS.".
           03  FILLER      PIC X(7)    VALUE " ".
           03  FILLER      PIC X(5)     VALUE "UNITS".
           03  FILLER      PIC X(8)     VALUE " ".
           03  FILLER      PIC X(6)     VALUE "AMOUNT".
           03  FILLER      PIC X(5)     VALUE " ".
           03  FILLER      PIC X(5)     VALUE "UNITS".
           03  FILLER      PIC X(7)     VALUE " ".
           03  FILLER      PIC X(6)     VALUE "AMOUNT".
           03  FILLER      PIC X(6)     VALUE " ".
           03  FILLER      PIC X(5)     VALUE "UNITS".
           03  FILLER      PIC X(8)     VALUE " ".
           03  FILLER      PIC X(6)     VALUE "AMOUNT".

       01  HEAD10.
           03  FILLER      PIC X(52)    VALUE
               "- - - - D E L I N Q U E N C Y   S T A T I S T I C S ".
           03  FILLER      PIC X(21)    VALUE
               "- - - -".

       01  HEAD11.
           03  FILLER      PIC X(11)    VALUE " ".
           03  FILLER      PIC X(21)    VALUE "---- CONTRACTUAL ----".
           03  FILLER      PIC X(05)    VALUE " ".
           03  FILLER      PIC X(21)    VALUE "------ RECENCY ------".
           03  FILLER      PIC X(26)    VALUE SPACES.
           03  FILLER      PIC X(41)    VALUE
               "- - - P R O C E E D S   V O L U M E - - -".

       01  HEAD12.
           03  FILLER      PIC X(12)    VALUE " ".
           03  FILLER      PIC X(5)     VALUE "UNITS".
           03  FILLER      PIC X(8)     VALUE " ".
           03  FILLER      PIC X(7)     VALUE "BALANCE".
           03  FILLER      PIC X(7)     VALUE " ".
           03  FILLER      PIC X(5)     VALUE "UNITS".
           03  FILLER      PIC X(8)     VALUE " ".
           03  FILLER      PIC X(7)     VALUE "BALANCE".
           03  FILLER      PIC X(11)    VALUE " ".
           03  FILLER      PIC X(11)    VALUE "LAST UPDATE".
           03  FILLER      PIC X(20)    VALUE SPACES.
           03  FILLER      PIC X(03)    VALUE "MTD".
           03  FILLER      PIC X(12)    VALUE SPACES.
           03  FILLER      PIC X(03)    VALUE "YTD".

       01  HEAD13.
           03  FILLER      PIC X(38)    VALUE
               "- - - - - - - - - - - - - - - - - - - ".
           03  FILLER      PIC X(38)    VALUE
               "- - - - - U N E A R N E D - - - - - - ".
           03  FILLER      PIC X(39)    VALUE
               "- - - - - - - - - - - - - - - - - - - -".

       01  HEAD14A.
           03  FILLER      PIC X(19)    VALUE "-------------------".
           03  FILLER      PIC X(19)    VALUE "-- NET GAIN / LOSS ".
           03  FILLER      PIC X(19)    VALUE "-------------------".
           03  FILLER      PIC X(19)    VALUE "  -----------------".
           03  FILLER      PIC X(19)    VALUE "----- VOLUME ------".
           03  FILLER      PIC X(19)    VALUE "-------------------".
       01  HEAD14B.
           03  FILLER      PIC X(19)    VALUE "           CURRENT ".
           03  FILLER      PIC X(19)    VALUE "    END-LAST-MONTH ".
           03  FILLER      PIC X(19)    VALUE "     END-LAST-YEAR ".
           03  FILLER      PIC X(19)    VALUE "     MONTH-TO-DATE ".
           03  FILLER      PIC X(19)    VALUE "      YEAR-TO-DATE ".
           03  FILLER      PIC X(19)    VALUE "         LAST-YEAR ".


       01  HEAD15.
           03  FILLER      PIC X(47)    VALUE
               "- - - - - - - - - U N I T S - - - - - - - - - -".
           03  FILLER      PIC X(5)    VALUE " ".
           03  FILLER      PIC X(43)    VALUE
               "- - - - - - - B A L A N C E S - - - - - - -".

       01  HEAD16.
           03  FILLER      PIC X(50)    VALUE
           "                     CURRENT  LAST MO  LAST YR    ".
           03  FILLER      PIC X(50)    VALUE
           "            CURRENT      LAST MO      LAST YR     ".

       01  HEAD17.
           03  FILLER      PIC X(50)    VALUE
           "                         MTD      YTD      LYR    ".
           03  FILLER      PIC X(50)    VALUE
           "                MTD          YTD          LYR     ".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-PROMPT-BRANCH  PIC X(7).
           03  FILLER           PIC X.
           03  D-BRANCH         PIC 9(4).
           03  FILLER           PIC X(3).
           03  D-PROMPT-DEALER  PIC X(7).
           03  FILLER           PIC X.
           03  D-DLNO           PIC 9(4).99.
           03  FILLER           PIC X.
           03  D-NAME           PIC X(25).
           03  FILLER           PIC XXX.
           03  D-PROMPT-CLASS   PIC X(11).
           03  FILLER           PIC X.
           03  D-CLASS          PIC 99.
           03  FILLER           PIC X.
           03  D-DESC           PIC X(17).

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  D-RESERVE    PIC Z,ZZZ,ZZZ.ZZ-.
           03  FILLER       PIC XX.
           03  D-OTHRESRV   PIC Z,ZZZ,ZZZ.ZZ-.
           03  FILLER       PIC XX.
           03  D-SVCHARGE   PIC Z,ZZZ,ZZZ.ZZ-.
           03  FILLER       PIC XX.
           03  D-SUMWGTTM   PIC ZZZZZZZZ.
           03  FILLER       PIC XX.
           03  D-SUMWGT     PIC ZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  FILLER       PIC X(10).
           03  D-DLDISC-PROCEEDS-VOLUME PIC X(43).
           03  D-DLDISC-VOL REDEFINES D-DLDISC-PROCEEDS-VOLUME.
               05  D-DLDISC-MTD   PIC Z,ZZZ,ZZZ.ZZ-.
               05  FILLER         PIC X(2).
               05  D-DLDISC-YTD   PIC Z,ZZZ,ZZZ.ZZ-.
               05  FILLER         PIC X(2).
               05  D-DLDISC-LYR   PIC Z,ZZZ,ZZZ.ZZ-.
           03  D-PROCEEDS-VOL REDEFINES D-DLDISC-PROCEEDS-VOLUME.
               05  FILLER         PIC X(8).
               05  D-PROCEEDS-MTD PIC ZZ,ZZZ,ZZZ.ZZ-.
               05  FILLER         PIC X(1).
               05  D-PROCEEDS-YTD PIC ZZ,ZZZ,ZZZ.ZZ-.
               05  FILLER         PIC X(6).

       01  D-LINE3 REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(50).
           03  FILLER      PIC X(5).
           03  D-RECOURSE                OCCURS 3 TIMES.
               04  D-RUNIT PIC ZZ,ZZZ-.
               04  FILLER  PIC X.
               04  D-RAMT  PIC Z,ZZZ,ZZZ.ZZ-.
               04  FILLER  PIC XXX.

       01  D-LINE4 REDEFINES DETAIL-LINE.
           03  D-PERIOD    PIC X(9).
           03  FILLER      PIC XX.
           03  D-CUNITS    PIC ZZ,ZZZ-.
           03  FILLER      PIC XX.
           03  D-COUTBAL   PIC Z,ZZZ,ZZZ.ZZ-.
           03  FILLER      PIC X(5).
           03  D-RUNITS    PIC ZZ,ZZZ-.
           03  FILLER      PIC XX.
           03  D-ROUTBAL   PIC Z,ZZZ,ZZZ.ZZ-.
           03  FILLER      PIC X(10).
           03  D-DELUNEDT  PIC 99/99/99.


       01  D-LINE6 REDEFINES DETAIL-LINE.
           03  D-TAG            PIC X(20).
           03  FILLER           PIC XX.
           03  D-UNITS                  OCCURS 3 TIMES.
               04  D-UNIT       PIC ZZ,ZZZ-.
               04  FILLER       PIC XX.
           03  FILLER           PIC X(8).
           03  D-BALANCES                OCCURS 3 TIMES.
               04  D-AMT2       PIC Z,ZZZ,ZZZ.ZZ-.

       01  D-LINE8 REDEFINES DETAIL-LINE.
           03  DISP-UNEARNED-AMT  PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ- OCCURS 6.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(37).
           03  RANGE-SELECTION.
               04  R-BRANCH     PIC 9(4).
               04  FILLER       PIC XX.
               04  R-DLNUM      PIC 9(6).
               04  FILLER       PIC XX.
               04  R-CLASS      PIC 99.
               04  FILLER       PIC XX.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y     W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/DSLIST_DEF.CPY".
       COPY "LIBLS/DSLIST_WKS.CPY".


      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
            COPY "LIBLS/DSLIST_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DS2-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-DL1-FILE.
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
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DSLIST" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE BEG-BRANCH TO BR-NO.
           PERFORM START-BR-FILE.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO DL-PATH-OVERRIDE
                                              DS-PATH-OVERRIDE.

           PERFORM OPEN-DS1-FILE.
           IF SEQ = 1
              MOVE HOLD-BEG-DATE-CCYYMM TO DS-DATE-CCYYMM
                                           QDS1-WBEG-DATE-CCYYMM
                                           QDS1-WEND-DATE-CCYYMM
              MOVE BR-NO TO DS-BRANCH
                            QDS1-WBEG-BRANCH
                            QDS1-WEND-BRANCH
              MOVE BEG-DLNUM TO DS-NUM
                                QDS1-WBEG-NUM
              MOVE END-DLNUM TO QDS1-WEND-NUM
              MOVE BEG-CLASS TO DS-CLASS
                                QDS1-WBEG-CLASS
              MOVE END-CLASS TO QDS1-WEND-CLASS
              PERFORM START-DS1-FILE
           ELSE
              MOVE BR-NO     TO DS-BRANCH
                                QDS2-WBEG-BRANCH
                                QDS2-WEND-BRANCH
              MOVE BEG-DLNUM TO DS-NUM
                                QDS2-WBEG-NUM
              MOVE END-DLNUM TO QDS2-WEND-NUM
              MOVE BEG-CLASS TO DS-CLASS
                                QDS2-WBEG-CLASS
              MOVE END-CLASS TO QDS2-WEND-CLASS
              PERFORM START-DS2-FILE.

           PERFORM OPEN-DL1-FILE.
       
       NEXT-DS.
           IF SEQ = 1
              PERFORM READ-DS1-FILE-NEXT
           ELSE
              PERFORM READ-DS2-FILE-NEXT.
           IF IO-FG = 9 OR (DS-PATH-OWNBR NOT = DS-BRANCH)
              GO TO END-OF-BRANCH-NO

           IF SEQ NOT = 1
              GO TO TEST-SEQ-2.

           IF ( DS-DATE-CCYYMM NOT = HOLD-BEG-DATE-CCYYMM )
              GO TO END-OF-BRANCH-NO.
           IF ( DS-BRANCH > END-BRANCH )
              GO TO END-OF-BRANCH-NO.
           IF ( DS-NUM < BEG-DLNUM ) OR ( DS-NUM > END-DLNUM )
              GO TO NEXT-DS.
           IF ( DS-CLASS < BEG-CLASS ) OR ( DS-CLASS > END-CLASS )
              GO TO NEXT-DS.
           GO TO SKIP-SEQ-2.

       TEST-SEQ-2.
           IF ( DS-BRANCH > END-BRANCH )
              GO TO END-OF-BRANCH-NO.
           IF DS-NUM > END-DLNUM
              GO TO NEXT-DS.
           IF ( DS-CLASS < BEG-CLASS ) OR ( DS-CLASS > END-CLASS )
              GO TO NEXT-DS.

       SKIP-SEQ-2.

           IF FIRST-TIME = "Y"
              MOVE "Y" TO RECORDS-FOUND
              MOVE "N" TO FIRST-TIME.

           PERFORM DETAIL-LINE-PROCESS.
           GO TO NEXT-DS.

       
       END-OF-BRANCH-NO.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-DL1-FILE.
           GO TO NEXT-BRANCH-NO.

       
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              GO TO END-PROGRAM.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE DSLIST-QUEUE-POS TO Q-POS.
           MOVE DSLIST-COPIES-POS TO C-POS.
           MOVE DSLIST-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM MOVE-BEG-TO-HOLD.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-LC1-FILE.

           MOVE 1 TO SUB.

           IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-LC1-FILE.

           IF ( SEQ = 2 )
              MOVE ZEROES    TO BEG-CLASS
              MOVE 99        TO END-CLASS.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           GO TO EXIT-INITIALIZE.
       
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO DISPLAY-MO.
           MOVE S-YY TO DISPLAY-YR.
           PERFORM SETUP-DS-DATE-CCYYMM.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              IF SEQ = 1
                 GO TO LC-02
              ELSE
                 GO TO DL-02.

       
       ENTER-SEQ.
           MOVE "ENNN010SEQ." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO ENTER-SEQ.
           MOVE VDUNUM0 TO SEQ.
           IF NOT (SEQ = 1 OR 2) GO TO ENTER-SEQ.
           IF SEQ = 1 GO TO SEND-MMYY.

           GO TO BR-01.

       
       SEND-MMYY.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO BR-01.
           PERFORM GET-TODAYS-DATE.
           PERFORM SETUP-DS-DATE-CCYYMM.
           GO TO BR-01.

       
       ENTER-DATE.
           MOVE "ENNN040MMYY." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO ENTER-SEQ.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO ENTER-DATE.
           MOVE VDUNUM0 TO DISPLAY-DATE.
           IF DISPLAY-DATE = 0 GO TO ENTER-DATE.
           IF DISPLAY-MO < 1 OR > 12 GO TO ENTER-DATE.
           PERFORM SETUP-DS-DATE-CCYYMM.

       
       BR-01.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-BR.
           IF UP-KEY
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO ENTER-SEQ
              ELSE
                 GO TO ENTER-DATE.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO BR-01.
           MOVE VDUNUM0 TO BEG-BRANCH.

       
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO BR-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-02.

           GO TO CHECK-BRANCH-SECURITY.

       
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

       
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       
       DL-01.
           MOVE "ENNN060DL1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY
              IF SEQ = 2
                 GO TO ENTER-SEQ
              ELSE
                 GO TO BR-02.
           IF F3-KEY GO TO ALL-DL.
           IF F7-KEY GO TO EXIT-PARM.
           IF F6-KEY
              MOVE "DBEG" TO DEALER-FG
              PERFORM DEALER-SCAN
              IF SCAN-ERR NOT = 0
              GO TO DL-01.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO DL-01.
           MOVE VDUNUM0 TO BEG-DLNUM.

       
       DL-02.
           MOVE "ENNN060DL2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO DL-01.
           IF F3-KEY GO TO ALL-DL.
           IF F7-KEY GO TO EXIT-PARM.
           IF F6-KEY
              MOVE "END" TO DEALER-FG
              PERFORM DEALER-SCAN
              IF SCAN-ERR NOT = 0
              GO TO DL-02.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO DL-02.
           MOVE VDUNUM0 TO END-DLNUM.

           IF BEG-DLNUM > END-DLNUM
              MOVE "INVALID DEALER RANGE!" TO MESS
              PERFORM SEND-MESS
              GO TO DL-02.

           IF SEQ = 2 GO EXIT-PARM.

           GO TO LC-01.
       ALL-DL.
           MOVE "SNNN060DL1." TO VDU.
           MOVE ZEROS TO VDUNUM0 BEG-DLNUM.
           PERFORM SCREENIO.
           MOVE "SNNN060DL2." TO VDU.
           MOVE 999999 TO VDUNUM0 END-DLNUM.
           PERFORM SCREENIO.
           IF SEQ = 2 GO EXIT-PARM.
           GO TO LC-01.
       
       LC-01.
           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-LC.
           IF UP-KEY GO TO DL-02.
           IF F7-KEY GO TO EXIT-PARM.
           IF F6-KEY
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF SCAN-ERR NOT = 0
              GO TO LC-01.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO LC-01.
           MOVE VDUNUM0 TO BEG-CLASS.

       
       LC-02.
           MOVE "ENNN020LC2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-LC.
           IF UP-KEY GO TO LC-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF F6-KEY
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF SCAN-ERR NOT = 0
              GO TO LC-02.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO LC-02.
           IF VDUNUM0 = 0 GO TO LC-02.
           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE!" TO MESS
              PERFORM SEND-MESS
              GO TO LC-02.

           GO TO EXIT-PARM.
       ALL-LC.
           MOVE "SNNN020LC1." TO VDU.
           MOVE ZEROS TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
       
           MOVE "SNNN010SEQ." TO VDU.
           MOVE SEQ TO VDUNUM0.
           PERFORM SCREENIO.

      * DATE IS IGNORED IN BATCH MAINT SO JUST PRINT SPACES

           MOVE "S  A040MMYY." TO VDU
           MOVE SPACES TO VDUBUF
           PERFORM SCREENIO.

           IF SEQ = 1
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO
           ELSE
              MOVE "S  A040BR1." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A040BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
      
           MOVE "SNNN060DL1." TO VDU.
           MOVE BEG-DLNUM TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060DL2." TO VDU.
           MOVE END-DLNUM TO VDUNUM0.
           PERFORM SCREENIO.

           IF SEQ = 1
              MOVE "SNNN020LC1." TO VDU
              MOVE BEG-CLASS TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN020LC2." TO VDU
              MOVE END-CLASS TO VDUNUM0
              PERFORM SCREENIO
            ELSE
              MOVE "S  A020LC1." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A020LC2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************
           PERFORM HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE "BRANCH:" TO D-PROMPT-BRANCH.
           MOVE DS-BRANCH TO D-BRANCH DL-BRNO.
           MOVE "DEALER:" TO D-PROMPT-DEALER.
           MOVE DS-DLNO TO D-DLNO DL-DLNO.
           PERFORM READ-DL1-FILE.
           IF IO-FG = 0
               MOVE DL-NAME TO D-NAME
           ELSE
               MOVE "**NO DEALER MSTR" TO D-NAME.

           MOVE "LOAN CLASS:" TO D-PROMPT-CLASS.
           MOVE DS-CLASS TO D-CLASS LC-CODE.
           PERFORM READ-LC1-FILE.

           IF IO-FG = 0
               MOVE LC-DESC TO D-DESC
           ELSE
               MOVE "**NO LOAN CL REC" TO D-DESC.

           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE HEAD5 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE HEAD6 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE DS-DLRVPRIN(1) TO D-RESERVE.
           MOVE DS-DLPARVHELD TO D-OTHRESRV.
           MOVE DS-DLDISC TO D-SVCHARGE.
           MOVE DS-SUMWGTTM TO D-SUMWGTTM.
           MOVE DS-SUMWGTAV TO D-SUMWGT.
           MOVE DS-DLDISC-VOL(1) TO D-DLDISC-MTD.
           MOVE DS-DLDISC-VOL(2) TO D-DLDISC-YTD.
           MOVE DS-DLDISC-VOL(3) TO D-DLDISC-LYR.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DS-DLRVPRIN(2) TO D-RESERVE.
           MOVE DS-UNPAIDCK TO D-SVCHARGE.
           MOVE DS-SUMWGTTA TO D-SUMWGT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           MOVE HEAD7 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE HEAD8 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE HEAD9 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 0 TO SUB.
           PERFORM PRINT-RECOURSE 3 TIMES.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           MOVE HEAD10 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE HEAD11 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE HEAD12 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE ZERO TO SUB.
           PERFORM PRINT-DELINQUENCY 7 TIMES.

           MOVE 2 TO LN-FEED.
           MOVE HEAD13 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED
           MOVE HEAD14A TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED
           MOVE HEAD14B TO PR-REC.
           PERFORM WRITE-PR-REC.

           PERFORM PRINT-EARNED.

           MOVE 2 TO LN-FEED.
           MOVE HEAD15 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE HEAD16 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO SUB.

       
       NEXT-LINE.

           MOVE 0 TO PCNTR.

           GO TO P-01 P-02 P-03 P-04 P-05 P-06 P-07 P-08 P-09
               DEPENDING ON SUB.

           GO TO END-DETAIL-LN-PROCESS.

       
       PRINT-LINE.
           MOVE TAG-ITEM (SUB) TO D-TAG.
           PERFORM WRITE-DETAIL-LINE.
           ADD 1 TO SUB.
           GO TO NEXT-LINE.

       P-01.
           PERFORM OUTSTANDING 3 TIMES.
           GO TO PRINT-LINE.

       P-02.
           PERFORM JUDGEMENTS 3 TIMES.
           GO TO PRINT-LINE.

       
       P-03.
           PERFORM CHARGE-OFF 3 TIMES.
           GO TO PRINT-LINE.

       
       P-04.
           PERFORM RECOVER 3 TIMES.
           GO TO PRINT-LINE.

       
       P-05.
           MOVE HEAD17 TO PR-REC.
           PERFORM WRITE-PR-REC.
           PERFORM NEW-PURCH 3 TIMES.
           GO TO PRINT-LINE.

       
       P-06.
           PERFORM OTH-PURCH 3 TIMES.
           GO TO PRINT-LINE.

       
       P-07.
           PERFORM CONVERTED 3 TIMES.
           GO TO PRINT-LINE.

       
       P-08.
           PERFORM PAID-OFF 3 TIMES.
           GO TO PRINT-LINE.

       
       P-09.
           PERFORM SERV-ACCTS 3 TIMES.
           GO TO PRINT-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ****************************
       PRINT-RECOURSE SECTION.
      ****************************

           ADD 1 TO SUB.
           MOVE DS-RCOURSE-UNITS (SUB) TO D-RUNIT (SUB).
           MOVE DS-RCOURSE-AMT (SUB) TO D-RAMT (SUB).

      *********************************
       PRINT-DELINQUENCY SECTION.
      *********************************
           ADD 1 TO SUB.
           MOVE DAYS-ITEM (SUB) TO D-PERIOD.

      * CONTRACTUAL
           MOVE DS-UNITS (1 2 SUB) TO D-CUNITS
           MOVE DS-OUTBAL (1 2 SUB) TO D-COUTBAL

      * RECENCY
           MOVE DS-UNITS (1 1 SUB) TO D-RUNITS
           MOVE DS-OUTBAL (1 1 SUB) TO D-ROUTBAL

           IF SUB = 1
               MOVE DS-DELUNEARN-DATE TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY TO D-DELUNEDT
               MOVE DS-PROCEEDS(1) TO D-PROCEEDS-MTD
               MOVE DS-PROCEEDS(2) TO D-PROCEEDS-YTD.

           PERFORM WRITE-DETAIL-LINE.

      ****************************************
       PRINT-EARNED SECTION.
      ****************************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE DS-UNEARNED-AMT(1 1 SUB) TO DISP-UNEARNED-AMT(1)
              MOVE DS-UNEARNED-AMT(2 1 SUB) TO DISP-UNEARNED-AMT(2)
              MOVE DS-UNEARNED-AMT(3 1 SUB) TO DISP-UNEARNED-AMT(3)

              MOVE DS-UNEARNED-AMT(1 2 SUB) TO DISP-UNEARNED-AMT(4)
              MOVE DS-UNEARNED-AMT(2 2 SUB) TO DISP-UNEARNED-AMT(5)
              MOVE DS-UNEARNED-AMT(3 2 SUB) TO DISP-UNEARNED-AMT(6)
              PERFORM WRITE-DETAIL-LINE
           END-PERFORM.


      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
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

       END-HEAD-PAGE.
           MOVE 2 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 4 TO LN-FEED.
           IF SEQ = 2
               GO TO NUM-RANGE.

           MOVE "BEGINNING BRANCH, DEALER, LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-BRANCH TO R-BRANCH.
           MOVE BEG-DLNUM TO R-DLNUM.
           MOVE BEG-CLASS TO R-CLASS.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING BRANCH, DEALER, LOAN CLASS" TO RANGE-LINE.
           MOVE END-BRANCH TO R-BRANCH.
           MOVE END-DLNUM TO R-DLNUM.
           MOVE END-CLASS TO R-CLASS.
           PERFORM WRITE-DETAIL-LINE.
           GO TO END-RANGE.

       NUM-RANGE.
           MOVE "               BEGINNING DEALER" TO RANGE-LINE.
           MOVE BEG-DLNUM TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "               ENDING DEALER" TO RANGE-LINE.
           MOVE END-DLNUM TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       END-RANGE.
           EXIT.

      *******************************************************
       CLASS-SCAN SECTION.
      *******************************************************
           MOVE 0      TO LCSCAN-KEY SCAN-ERR.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO SCAN-ERR
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
               MOVE "SNNN020LC1." TO VDU
               MOVE LCSCAN-KEY TO BEG-CLASS VDUNUM0
               PERFORM SCREENIO
           ELSE
               MOVE "SNNN020LC2." TO VDU
               MOVE LCSCAN-KEY TO END-CLASS VDUNUM0
               PERFORM SCREENIO.

       EXIT-CLASS-SCAN.
           EXIT.

      *********************************************************
       DEALER-SCAN SECTION.
      *********************************************************
           MOVE 0 TO DLSCAN-NUM.
           MOVE "WI/DLSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF DLSCAN-STATUS NOT = "00" OR DLSCAN-NUM = " "
               MOVE 9 TO  SCAN-ERR
               GO TO EXIT-DEALER-SCAN.
           IF ( DEALER-FG = "DBEG" )
               MOVE "SNNN060DL1." TO VDU
               MOVE DLSCAN-NUM TO BEG-DLNUM VDUNUM0
               PERFORM SCREENIO
           ELSE
               MOVE "SNNN060DL2." TO VDU
               MOVE DLSCAN-NUM TO END-DLNUM VDUNUM0
               PERFORM SCREENIO.

       EXIT-DEALER-SCAN.
           EXIT.

      *************************************
       SETUP-DS-DATE-CCYYMM SECTION.
      *************************************
           MOVE DISPLAY-MO TO DATE-MMDDYY-MM.
           MOVE DISPLAY-YR TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM HOLD-BEG-DATE-CCYYMM.
       SETUP-DS-DATE-EXIT.
           EXIT.

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO DSLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DSLIST-SYSDATE.
           DISPLAY DSLIST-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DSLIST-FORM-FIELDS TO WR-BUF.
           MOVE DSLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE DSLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE DSLIST-HELP--DIR  TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
                COPY "LIBLS/DSLIST_EVA.CPY".
                WHEN OTHER MOVE "2A" TO STAT
           END-EVALUATE.


      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       OUTSTANDING.
           ADD 1 TO PCNTR.
           MOVE DS-OUTSUNIT (PCNTR) TO D-UNIT (PCNTR).
           MOVE DS-OUTSAMT (PCNTR) TO D-AMT2 (PCNTR).
       NEW-PURCH.
           ADD 1 TO PCNTR.
           MOVE DS-NPURUNIT (PCNTR) TO D-UNIT (PCNTR).
           MOVE DS-NPURAMT (PCNTR) TO D-AMT2 (PCNTR).
       OTH-PURCH.
           ADD 1 TO PCNTR.
           MOVE DS-OPURUNIT (PCNTR) TO D-UNIT (PCNTR).
           MOVE DS-OPURAMT (PCNTR) TO D-AMT2 (PCNTR).
       CONVERTED.
           ADD 1 TO PCNTR.
           MOVE DS-RNEWUNIT (PCNTR) TO D-UNIT (PCNTR).
           MOVE DS-RNEWAMT (PCNTR) TO D-AMT2 (PCNTR).
       JUDGEMENTS.
           ADD 1 TO PCNTR.
           MOVE DS-JDOSUNIT (PCNTR) TO D-UNIT (PCNTR).
           MOVE DS-JDOSAMT (PCNTR) TO D-AMT2 (PCNTR).
       CHARGE-OFF.
           ADD 1 TO PCNTR.
           MOVE DS-PLOSUNIT (PCNTR) TO D-UNIT (PCNTR).
           MOVE DS-PLOSAMT (PCNTR) TO D-AMT2 (PCNTR).
       RECOVER.
           ADD 1 TO PCNTR.
           MOVE DS-RCPLUNIT (PCNTR) TO D-UNIT (PCNTR).
           MOVE DS-RCPLAMT (PCNTR) TO D-AMT2 (PCNTR).
       PAID-OFF.
           ADD 1 TO PCNTR.
           MOVE DS-PAIDUNIT (PCNTR) TO D-UNIT (PCNTR).
           MOVE DS-PAIDAMT (PCNTR) TO D-AMT2 (PCNTR).
       SERV-ACCTS.
           ADD 1 TO PCNTR.
           MOVE DS-TODLUNIT (PCNTR) TO D-UNIT (PCNTR).
           MOVE DS-TODLAMT (PCNTR) TO D-AMT2 (PCNTR).
       MOVE-BEG-TO-HOLD.
           MOVE BEG-BRANCH TO HOLD-BEG-BRANCH.
           MOVE BEG-DLNUM  TO HOLD-BEG-NUM.
           MOVE BEG-CLASS  TO HOLD-BEG-CLASS.
       GET-TODAYS-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           MOVE ALP-MO TO DISPLAY-MO.
           MOVE ALP-YR TO DISPLAY-YR.
           MOVE "SNNN040MMYY." TO VDU.
           MOVE DISPLAY-DATE TO VDUNUM0.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPDS1RN.CPY".
       COPY "LIBLP/LPDS2RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
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
             IF EXT-ROUTE-BUF = "00" OR "70"
                 MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
                 PERFORM SEND-MESS.

       
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY DSLIST-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
