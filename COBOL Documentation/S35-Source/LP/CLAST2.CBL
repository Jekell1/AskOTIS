       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CLAST2.
       DATE-WRITTEN. 10-13-94
      ****************************************************************
      *
      *    DIR :  LP
      *    DESC:  CLASS TRANSFER UPDATE
      *           TRANSFER LOANS SELECTED IN CLAST1.C
      *           RE: WORK FILE
      *
      *=================================================================
      * REV:
      * 101494 BAH REMOVED ALL OCCURANCES OF LP2
      * 021795 JTG CORRECTED NOT TRANSFERING SECONDARY MAKERS
      * 042195 JTG ADDED SET OF UNEARN'S FOR LPEOD5.C UPDATE
      * 061695 JTG ADDED SET OF CRNORC-BR-NOT-OURS-REFUND
      * 062895 MJD ADDED COPY MEMBERS LIBLP/POSTDATE AND POSTDATEW.
      *            SET LP-USERID, POSTTIME, AND POSTDATE.
      *            SET TJ-USERID, POSTTIME, AND POSTDATE.
      * 071995 JTG ADDED LOGIC FOR LN-PRLNAMT & LN-PDPDUE-DAYS
      * 072195 JTG ADDED LPSRCD LOGIC
      * 072895 JTG ADDED LOGIC FOR DELSUB-SET ROUTINE
      * 081195 MJD ADDED GP-FILE INFO FOR LIBLP/LPSRCD :
      *                  GP-THE-ONLY-SF-SRCD
      * 081395 JTG CHANGED TO PICKUP WK-TO-DLNO FROM WK-REC
      * 083195 KEC ADDED LIBLP/LPNOCL
      * MJD 091595 ADDED DS-DATE LOGIC.
      * JTG 111395 CHANGED TO UPDATE TO->BORROWERS AND TO->BORR MEMO
      *            BEFORE TO->LOAN INFO, SO UPDATE OF BI-FILE WOULD
      *            FIND THE CORRESPONDING BORROWER INFO
      * MJD 012596 ADDED LOGIC TO UPDATE LOANS SOLD/TRANS/PURCH,
      *            AND ADDED LOGIC FOR UPDATE OF SEPARATE PL TABLE.
      * MJD 030496 ADDED E-CODE TEST OF "22" ON WRITE-TY1-FILE.
      * MJD 031196 ADDED LOGIC TO STOP 0 AS ACCOUNT NUMBER.
      * MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      * MJD 071896 ADDED LOGIC FOR WEEKLY UNITPER.
      * JTG 081396 CHANGED TO INSURE THAT NO DUPLICATE LN-SSNO'S EXIST
      * JTG 091896 ALLOWED TRANSFER OF P&L ACCOUNTS
      *            ADDED COPY OF "LIBLP/CLASTRW"
      * MJD 102596 MOVED 'U' TO STAT TO PROHIBIT PIGGYBACK PRINTING.
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER READING BWFILE.
      * JTG 030797 CORRECTED BUG IN UPDATE OF DS RECOURSE STATISTICS
      * JTG 030797 ALLOWED TRANSFER OF REPO ACCOUNTS
      * MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      * GLF 090297 REMOVED ALL USE OF GLOBAL
      * GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * MJD 100997 REMOVED REFERENCE TO DELSUB. CHANGED DS-DELINQ TABLE
      *            FROM ACCOURS 18 TO OCCURS 10.
      * JTG 120197 CORRECTED TO UPDATE DELINQUENCY WITH OUTBAL-LESS-O2
      * MJD 032098 ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      * MJD 051998 REMOVED XX-PASTDUE. CHANGED XX-EXREB TO BE XX-PART-CHGOFF
      * JTG 061598 ADDED LOGIC TO UPDATE BANKRUPT DELINQUENT STATISTICS
      * GLF 110698 MADE MEMO DATE 2000 COMPATIBLE
      * MJD 990416 CHANGED TO INCLUDE/EXCLUDE O2 IN DELINQ BASED
      *            ON GP-DELINQ-WITH-O2
      * BLV 990819 CHANGED TO UPDATE BUCKETS 4 & 5 OF BY & DS REPO TABLE
      *            WITH REPOS IN PL REC'BLE, TFC PR#20, GR SUBURBAN PR# 809
      * GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *            MEETING (082599)
      * BAH 991230 ADDED OPTION TO I,S,O ZERO BALANCE ACCOUNTS, TFC PR# 62
      *            DONT ALLOW CLASS TRANSFER ON ACCOUNTS IN RESCISSION
      * BAH 000110 AGEING TOTALS WERE OLD SUBSCRIPTS FROM PRIOR TO
      *            WORLD, CAUSING INDEX OUT OF BOUNDS LOCK UPS !!
      *            TENFIN (TN) JUDY
      * JTG 000411 MODIFIED TO UPDATE UNEARNED STATISTICS, MERCURY #223
      * BLV 000713 CHANGED TO UPDATE BUCKETS 4 & 5 OF BY & DS REPO TABLE
      *            REGARDLESS OF REPO DATE OR PL DATE, TFC PR# 77
      * BLV 000925 ADDED DELETE OF WORK RECORD TO EXCEPTION ROUTINE; IF
      *            EXCEPTIONS FOUND, EVERY EXCEPTION WOULD PRINT FOR
      *            EVERY SUBSEQUENT RECORD IN THE WORK FILE!! ALSO,
      *            FIX ACCT# IN EXCEPTION PRINT, 1ST 2 DIGITS TRUNCATED
      * BAH 010626 WAS NOT MOVING TO-DLNO TO LN-DLNO, WAS CHANGING ALL
      *            THE OTHER FILES THOUGH !! FOUND MYSELF
      * BAH 031118 BR-OPEN-FG WAS BEING TESTED FOR "E", BUT WAS SET TO "N"
      *            IF DAY WAS NOT OPEN.....??? GENIUS !! LORI,PIKCO
      *            CAUSED STATS IN TY TO NEVER GET UPDATED INTO BY
      * BAH 040819 DO NOT CREATE FORM REQUEST IF LOAN OR BORROWER IS
      *            CONFIDENTIAL, PR# 2343
      * BLF 050603 PUT ANOTHER CHECK TO VERIFY DAY IS OPEN FOR TO AND
      *            FROM BRANCHES, AFTER ENTER-PARAMETERS, RIGHT BEFORE
      *            TAG IS CREATED, PL# 495
      *  CS 081126 ADD TEST GP-INCL-OTH-IN-EARNED = 'R'. IF 'R' DONT INCLUDE
      *            OTHER(ANTICERN(6) OR MAINTFEE(ANTICERN(4) IN ANTICIPATED
      *                                                  REGACC PR#210
      *  CS 101109 ADDED 'FROM' CLASS RANGE FOR ALL LIKE CLASSES TRANSFERRED
      *            TO SAME DESTINATION CLASS.   REGIONAL ACEPT PR#277
      *  CS 140520 REMOVED LIBGB/RM_CMD.CPY AND REPLACED WITH REMOVE-
      *            WORKFILE
      *  CS 150427 TEST THE RC FOR OPEN-DAY, PROGRAM DOES NOT OPEN OP SO
      *            DID NOT TEST FOR THE OP-FG.
      * KEC 2015.1120 FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      * BAH 160128 SPLIT OUT LTFILE, PD0003
      * BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      * KEC 2016.0219 FIXED KEYS & RECS FOR FD-REC.
      *               CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * BAH 160224 FIX FOR NEW TY-DATE FORMAT
      * BLM 160415 FIX BUG; CLASS TRANSFER WAS UPDATING PAID-OUT ACCOUNTS TO
      *            NUMBER OF ACCOUNTS IN BRANCH & DEALER STATS, WORLD PR# 1152
      * BLM 160511 FIX FOR NEW DS-DATE FORMAT, FIX DS-UNEARED, PD0003
      * BLM 160811 THERE WERE MORE PLACES TO CHECK IF ACCOUNT WAS PAIDOUT,
      *             FIXED OTHER FIELDS THAT WERE STILL GETTING UPDATED,
      *             WORLD PR# 1152A
      * KEC 2016.1026 REMOVED USING HOLD-TR1-KEY.
      *               ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      * KEC 2017.0105 CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *               WITH THE DATE FIELDS IN BY & UD FILE.
      * KEC 2017.0425 REMOVED 'LE' FILE SINCE NOT USED.
      * BAH 2017.0517 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0608 ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 2018.1003 GLOBAL BWFILE
      * BAH 2018.1114 GLOBAL FMFILE
      * BAH 2019.0313 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      * BAH 2019.0614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 2024.0130 CALL CRNOR2 AND WRITE NOTICE RECORD
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBLP/LPFSCLAS.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBLP/LPFDCLAS.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/CLAST2.CBL S35 02/20/24-13:20:01 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
      * MOVES LN-REC TO PD-RECORD FOR STORAGE
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LPWSIX.CPY".
       COPY "LIBLP/LPWSCLAS.CPY".
      *-----------------------------------------------------------------
       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LP01TRJ_SQL.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
      *-----------------------------------------------------------------
       COPY "LIBLP/LPEDPATH.CPY".

       01  PR-IFN          PIC X(2) VALUE "PR".
       01  WK-IFN          PIC X(2) VALUE "WK".

       COPY "LIBLP/CLASTRW.CPY".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED              PIC 99    VALUE 0.
       01  LN-COUNT             PIC 999   VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)  VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  SUMMARY-FG           PIC X        VALUE "N".
       01  BR-OPEN-FG           PIC X.

       01  PL-ONLY              PIC X        VALUE " ".
       01  ZBAL                 PIC X        VALUE " ".
       01  ONHAND               PIC X.
       01  SOLD                 PIC X.
       01  REDEEM               PIC X.
       01  CHG-SPR-LC           PIC X        VALUE " ".
       01  BUILD-SPR            PIC X        VALUE " ".

       01  ERR-FG               PIC X        VALUE " ".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  REC-EXISTS           PIC X.
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).
       01  TRANS-DATE           PIC 9(8)     COMP-3 VALUE 0.
       01  NUM-EDIT             PIC ZZZZZ9.99.
       01  BW-SUB               PIC 999.
       01  HOLD-LP-PDTH-DATE    PIC 9(8).
       01  HOLD-LP-IBPC         PIC X.
       01  HOLD-TJ-LINE         PIC 999.
       01  PART-RC-AMT          PIC S9(8)V99.
       01  TEST-LN-CLASS        PIC 9(3).
       01  HOLD-FROM-LN-CLASS   PIC 9(3).
       01  REF.
           03  REF-FRLC         PIC 9(2).
           03  REF-X            PIC X(1).
           03  REF-TOLC         PIC 9(2).
       01  SAVE-WORKERS.
           03  SAVE-UNEARN      OCCURS 6 TIMES
                                PIC S9(7)V99   COMP.

       01  HOLD-BRNO            PIC 9999     VALUE 0.

       01  HOLD-FM1-KEY         PIC X(28)     VALUE SPACES.

       01  HOLD-LPS-NUM.
           03  H-LPS-CLASS      PIC XXX.
           03  H-LPS-NUM        PIC X(13).

       01  SAVE-RDSUB           PIC 99       VALUE 0.
       01  SAVE-CSUB            PIC 99       VALUE 0.
       01  SAVE-PSUB            PIC 99       VALUE 0.
       01  SAVE-OUTBAL          PIC S9(7)V99 VALUE 0.

       01  UNEARN-INT           PIC S9(7)V99 COMP-3 VALUE 0.
       01  UNEARN-SC            PIC S9(7)V99 COMP-3 VALUE 0.
       01  UNEARN-MF            PIC S9(7)V99 COMP-3 VALUE 0.

       01  EXCEPTION-MISSING    PIC X(43) VALUE
           "                  MISSING SINCE SELECTION!!".
       01  EXCEPTION-VOID       PIC X(44) VALUE
           "                    VOIDED SINCE SELECTION!!".
       01  EXCEPTION-RESC       PIC X(44) VALUE
           "                RESCISSION SINCE SELECTION!!".
       01  EXCEPTION-BT         PIC X(44) VALUE
           "          NO LONGER BULKED SINCE SELECTION!!".
       01  EXCEPTION-PL         PIC X(39) VALUE
           "                  P&L SINCE SELECTION!!".
       01  EXCEPTION-NOT-PL     PIC X(43) VALUE
           "                  NOT P&L SINCE SELECTION!!".
       01  EXCEPTION-ZERO       PIC X(44) VALUE
           "                  ZERO BAL SINCE SELECTION!!".
       01  EXCEPTION-NOT-ZERO   PIC X(48) VALUE
           "                  NOT ZERO BAL SINCE SELECTION!!".
       01  EXCEPTION-REPO       PIC X(40) VALUE
           "                  REPO SINCE SELECTION!!".
       01  EXCEPTION-CLASS      PIC X(40) VALUE
           "                  PROBLEM: FROM CLASS!!".

      ********************************************
      *               T O T A L S                *
      ********************************************
      *
      *    TOTAL-TBL:
      *       LEV(1) =  BY LOAN CLASS
      *          (2) =  BY BRANCH
      *          (3) =  BY COMPANY
      *
      *    T-AGETBL :
      *          (1) =   30   DAYS OLD
      *          (2) =   60   DAYS OLD
      *          (3) =   90   DAYS OLD
      *          (4) =  120   DAYS OLD
      *          (5) =  150   DAYS OLD
      *          (6) =  180   DAYS OLD
      *          (7) =  210 + DAYS OLD
      *          (8) =  <M>   MISS PAYS
      *          (9) =  <P>   POTENTIALS
      *         (10) =  CURRENT
      *
      *    T-TYPETBL:
      *       TYP(1) = CONTRACTUAL
      *          (2) = RECENCY
      *          (3) = PAID AHEAD
      *                CONTRACTUALLY CURRENT
      *                    AND DELINQ-RECENCY
      *
      ********************************************
       01  TOTAL-TABLE            VALUE ZEROS.
           03  TOTAL-TBL          OCCURS 3 TIMES.
               05  T-AGETBL       OCCURS 10 TIMES.
                   07  T-TYPETBL  OCCURS 3 TIMES.
                       09  T-NO   PIC S9(5).
                       09  T-BAL  PIC S9(9)V99.
                       09  T-INT  PIC S9(7)V99.
                       09  T-OTH  PIC S9(7)V99.
                       09  T-UNE  PIC S9(7)V99.
                       09  T-ERN  PIC S9(7)V99.
                       09  T-IER  PIC S9(7)V99.
       01  TT-TOTALS.
           03  TT-TYPETBL         OCCURS 3 TIMES.
               05  TT-NOS         PIC S9(5)    COMP-3.
               05  TT-BAL         PIC S9(9)V99 COMP-3.
               05  TT-INT         PIC S9(7)V99 COMP-3.
               05  TT-OTH         PIC S9(7)V99 COMP-3.
               05  TT-UNE         PIC S9(7)V99 COMP-3.
               05  TT-ERN         PIC S9(7)V99 COMP-3.
               05  TT-IER         PIC S9(7)V99 COMP-3.

       01  LEV                    PIC 9(6) COMP-3.
       01  AGE                    PIC 9(6) COMP-3.
       01  TYP                    PIC 9(6) COMP-3.
       01  SUB                    PIC 9(6) COMP-3.

       01  DD-TOTAL-TABLE.
           03  DD-TOT-TBL         OCCURS 32 TIMES.
               05  DD-NO          PIC S9(5)    COMP-3.
               05  DD-BAL         PIC S9(9)V99 COMP-3.
               05  DD-INT         PIC S9(7)V99 COMP-3.
               05  DD-OTH         PIC S9(7)V99 COMP-3.
               05  DD-UNE         PIC S9(7)V99 COMP-3.

       01  NM-TOTAL-TABLE.
           03  NM-TOT-TBL         OCCURS 27 TIMES.
               05  NM-NO          PIC S9(5)    COMP-3.
               05  NM-BAL         PIC S9(9)V99 COMP-3.
               05  NM-INT         PIC S9(7)V99 COMP-3.
               05  NM-OTH         PIC S9(7)V99 COMP-3.
               05  NM-UNE         PIC S9(7)V99 COMP-3.

       01  AGE-DESC-TABLE.
           03  FILLER             PIC X(12) VALUE "    30 DAYS:".
           03  FILLER             PIC X(12) VALUE "    60 DAYS:".
           03  FILLER             PIC X(12) VALUE "    90 DAYS:".
           03  FILLER             PIC X(12) VALUE "   120 DAYS:".
           03  FILLER             PIC X(12) VALUE "   150 DAYS:".
           03  FILLER             PIC X(12) VALUE "   180 DAYS:".
           03  FILLER             PIC X(12) VALUE "   210 +   :".
           03  FILLER             PIC X(12) VALUE "   <M>     :".
           03  FILLER             PIC X(12) VALUE "   <P>     :".
           03  FILLER             PIC X(12) VALUE "   CURRENT :".
       01  AGE-DESC-TBL           REDEFINES AGE-DESC-TABLE.
           03  AGE-DESC           OCCURS 10 TIMES
                                  PIC X(12).

       01  ERRCD                  PIC X.
       01  FORMNO                 PIC 9(4).

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
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(24)    VALUE " ".
           03  FILLER           PIC X(27)    VALUE
           "CLASS TRANSFER UPDATE FROM ".
           03  H-FRLC           PIC Z9.
           03  H-DASH           PIC X VALUE SPACES.
           03  H-FR2LC          PIC Z9.
           03  FILLER           PIC X(4)     VALUE
           " TO ".
           03  H-TOLC           PIC Z9.

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC 9999.
           03  H-BRNOX     REDEFINES H-BRNO PIC XXXX.
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(20).
           03  FILLER      PIC X(26)    VALUE
           "           TFR TO DEALER: ".
           03  H-TO-DLNO   PIC 9999.99.
           03  FILLER      PIC X(44)    VALUE " ".
           03  FILLER      PIC X(20)    VALUE
           "99.99.99 - LOAN DATE".


       01  HEAD5.
           03  FILLER      PIC X(48)    VALUE " ".
           03  HEAD5XX.
               05  FILLER     PIC X(31)    VALUE
               "LAST  REC-    RECENCY  UNEARNED".
               05  FILLER     PIC X(15)    VALUE
               "   CONTRACTUAL".
           03  FILLER      PIC X(38)    VALUE
               "    PAID   -------  BALANCES  -------".

       01  HEAD6.
           03  FILLER      PIC X(39)    VALUE
               "ACCOUNT NO.  DUE  BORROWER".
           03  FILLER      PIC X(45)    VALUE
               "DEALER PAYMENT ENCY    PROFILE   CHARGES    P".
           03  FILLER      PIC X(48)    VALUE
               "ROFILE   AGE  THRU  OTH-DUE INT/LC-DUE PRINCIPAL".

       01  DETAIL-LINE        PIC X(132) VALUE SPACES.
       01  D-LINE1            REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM     PIC X(13).
           03  FILLER         REDEFINES D-LOAN-NUM.
               05  D-NUMBER   PIC 9(6).
               05  FILLER     PIC X(7).
           03  FILLER         PIC X.
           03  D-DUEDAY       PIC 99.
           03  FILLER         PIC XX.
           03  D-NAME         PIC X(19).
           03  FILLER         PIC X.
           03  D-DLNO         PIC ZZZ9.99.
           03  FILLER         PIC X.
           03  D-PAYDATE      PIC 99/99/99.
           03  FILLER         REDEFINES D-PAYDATE.
               05  FILLER     PIC XX.
               05  D-DASH1    PIC X.
               05  FILLER     PIC XX.
               05  D-DASH2    PIC X.
               05  FILLER     PIC XX.
           03  D-RECENCYX.
               05  D-RECENCY  PIC ZZZZ.
           03  FILLER         PIC X.
           03  D-PYRECPRO     PIC 9(12).
           03  D-XPYRECPRO    REDEFINES D-PYRECPRO
                              PIC X(12).
           03  D-UNEARNED     PIC ZZZZ9.99-.
           03  D-PROFILE      PIC 9(12).
           03  D-XPROFILE     REDEFINES D-PROFILE
                              PIC X(12).
           03  D-CONTUALX.
               05  D-CONTUAL  PIC ZZZZ.
           03  D-PDTHRU       PIC ZZ9999.
           03  D-OTHBAL       PIC ZZZZZ9.99-.
           03  D-INTBAL       PIC ZZZZZ9.99-.
           03  D-CURBAL       PIC ZZZZZ9.99-.

       01  SUMMARY-LINE1 REDEFINES DETAIL-LINE.
           03  S-SUMNAME        PIC X(20).
       01  SUMMARY-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(46).
           03  S-AGETYPE        PIC X(80).
       01  SUMMARY-LINE3 REDEFINES DETAIL-LINE.
           03  S-NAME           PIC X(12).
           03  S-NO             PIC ZZZZ9-.
           03  S-NO-P           PIC ZZ9.99-.
           03  S-OTH            PIC ZZZZZ9.99-.
           03  S-OTH-P          PIC ZZ9.99-.
           03  S-INT            PIC ZZZZZ9.99-.
           03  S-INT-P          PIC ZZ9.99-.
           03  S-BAL            PIC ZZZZZZZ9.99-.
           03  S-BAL-P          PIC ZZ9.99-.
           03  S-UNE            PIC ZZZZZZ9.99-.
           03  S-UNE-P          PIC ZZ9.99-.
           03  S-IER            PIC ZZZZZZ9.99-.
           03  S-IER-P          PIC ZZ9.99-.
           03  S-ERN            PIC ZZZZZZ9.99-.
           03  S-ERN-P          PIC ZZ9.99-.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(112).

       01  SUMMARY1.
           03  FILLER           PIC X(15)  VALUE SPACES.
           03  FILLER           PIC X(48)  VALUE
           "NO----%     OTH DUE----%  INT/LC DUE----%".
           03  FILLER           PIC X(17)  VALUE
           "BAL DUE----%   UN".
           03  FILLER           PIC X(51)  VALUE
           "EARN-INT----%    UNEARN-SC----%    UNEARN-MF----%".

       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBWI/FDSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/LPSRCDW.CPY".
       01  CLAST2-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CLAST2_DEF.CPY".
       COPY "LIBLP/CLAST2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  TRANSFER-BUF.
           03  FR-LC              PIC 9(3).
           03  FR-LC-X           REDEFINES FR-LC PIC X(3).
           03  FR2-LC             PIC 9(3).
           03  FR2-LC-X          REDEFINES FR2-LC PIC X(3).
           03  TO-LC              PIC 9(3).
           03  TO-DLNO            PIC 9(4)V99.


       SCREEN SECTION.
       COPY "LIBLP/CLAST2_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME TRANSFER-BUF EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-TRJ1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CLAST2" TO VDU-FORM-NAME FORM-PROG.

      * DONT NEED SECURITY CHECK IF YOU GOT THRU CLASTR!

      * MADE FLOATING SO IT CAN RETURN TO CLASTR STILL FILLED OUT
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CLAST2-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM CLAS-TRANSFER.

           IF FIRST-TIME = "N"
              PERFORM COMPANY-TOTALS.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************
       CLAS-TRANSFER SECTION.
      ******************************************
           PERFORM OPEN-WK-FILE.

           MOVE 0 TO WK-ACCTNO.
       CLAS-TRANSFER-START.
           MOVE "D" TO WK-TYPE.
           PERFORM START-WK-FILE.
       CLAS-TRANSFER-NEXT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              PERFORM CLOSE-WK-FILE
              MOVE CLAS-PATH TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE
              IF NOT (PRU-LP = "VDU" OR "PRU")
                 MOVE "CLASS TRANSFER UPDATE IS COMPLETE!!!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO CLAS-TRANSFER-EXIT
              ELSE
                 GO TO CLAS-TRANSFER-EXIT.

           PERFORM OPEN-LN1-FILE.
           MOVE EXT-CONAME-USER-LOC TO LN-OWNBR.
           MOVE WK-ACCTNO           TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-BAD
              MOVE EXCEPTION-MISSING TO DETAIL-LINE
              PERFORM EXCEPTION-ROUTINE
              GO TO CLAS-TRANSFER-NEXT.

           IF LN-POCD-VOID
              MOVE EXCEPTION-VOID TO DETAIL-LINE
              PERFORM EXCEPTION-ROUTINE
              GO TO CLAS-TRANSFER-NEXT.

           IF LN-POCD-INRESCISSION OR LN-POCD-RESCIND OR LN-POCD-PB-RESC
              MOVE EXCEPTION-RESC TO DETAIL-LINE
              PERFORM EXCEPTION-ROUTINE
              GO TO CLAS-TRANSFER-NEXT.

           IF LN-POCD = "BT"
              MOVE EXCEPTION-BT TO DETAIL-LINE
              PERFORM EXCEPTION-ROUTINE
              GO TO CLAS-TRANSFER-NEXT.

           MOVE LN-CLASS TO TEST-LN-CLASS.
           PERFORM VERIFY-CLASS.
           IF STAT NOT = "00"
              MOVE EXCEPTION-CLASS TO DETAIL-LINE
              PERFORM EXCEPTION-ROUTINE
              GO TO CLAS-TRANSFER-NEXT.

      * TEST FOR OPTION OF ZERO BALANCE ONLY:
           IF ZBAL = "O"
              IF LN-CURBAL NOT = 0
                 MOVE EXCEPTION-NOT-ZERO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO CLAS-TRANSFER-NEXT
              END-IF
           ELSE
      * TEST FOR OPTION OF NO ZERO BALANCE ACCOUNTS:
           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 MOVE EXCEPTION-ZERO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO CLAS-TRANSFER-NEXT.

      * TEST FOR OPTION OF P&L ONLY:
           IF PL-ONLY = "O"
              IF LN-PLCD = " " OR LN-PLDATE = 0
                 MOVE EXCEPTION-NOT-PL TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO CLAS-TRANSFER-NEXT
              END-IF
           ELSE
      * TEST FOR OPTION OF NO P&L ACCOUNTS:
           IF PL-ONLY = "S"
              IF (LN-PLCD NOT = " ") OR (LN-PLDATE NOT = 0)
                 MOVE EXCEPTION-PL TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO CLAS-TRANSFER-NEXT.

      * TEST FOR OPTION OF REPO ACCOUNTS:
           IF ONHAND = "O"
              IF LN-REPOCD NOT = "X"
                 MOVE EXCEPTION-REPO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO CLAS-TRANSFER-NEXT.

           IF SOLD = "O"
              IF LN-REPOCD NOT = "S"
                 MOVE EXCEPTION-REPO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO CLAS-TRANSFER-NEXT.

           IF REDEEM = "O"
              IF LN-REPOCD NOT = "V"
                 MOVE EXCEPTION-REPO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO CLAS-TRANSFER-NEXT.

           IF ONHAND = "S"
              IF LN-REPOCD = "X"
                 MOVE EXCEPTION-REPO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO CLAS-TRANSFER-NEXT.

           IF SOLD = "S"
              IF LN-REPOCD = "S"
                 MOVE EXCEPTION-REPO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO CLAS-TRANSFER-NEXT.

           IF REDEEM = "S"
              IF LN-REPOCD = "V"
                 MOVE EXCEPTION-REPO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO CLAS-TRANSFER-NEXT.

           IF NOT (PRU-LP = "VDU" OR "PRU")
              MOVE " " TO MESS
              STRING "PROGRAM IN PROGRESS..." LN-OWNBR " " PD-ACCTNO
                      DELIMITED BY SIZE INTO MESS
              PERFORM SEND-LEGEND.

      * SAVE UNEARNED LOAN AMOUNTS:
      
           PERFORM COMPUTE-EARNINGS.
           MOVE EARN-UNEARN(1) TO SAVE-UNEARN(1).
           MOVE EARN-UNEARN(2) TO SAVE-UNEARN(2).
           MOVE EARN-UNEARN(3) TO SAVE-UNEARN(3).
           MOVE EARN-UNEARN(4) TO SAVE-UNEARN(4).
           MOVE EARN-UNEARN(5) TO SAVE-UNEARN(5).
           MOVE EARN-UNEARN(6) TO SAVE-UNEARN(6).

      * STORE PICTURE OF LOAN RECORD IN SCRATCH:
           MOVE LN-REC TO PD-RECORD.

      * ESTABLISH CONTRACTUAL/RECENCY FOR UPDATE OF TY & DS
           PERFORM GET-SP.
           PERFORM ESTABLISH-ORIG-TY-DS-NUMBERS.

      * GET PAID THRU AND IBPC-FG
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO HOLD-LP-PDTH-DATE.
           MOVE LN-1STPYDATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG TO HOLD-LP-IBPC.

      * CHANGE CLASS LN-FILE:
           PERFORM CHANGE-CLASS-LN-FILE.

      * UPDATE NOTICE RECORD IN CASE SPR IS DIFFERENT IN NEW CLASS
           PERFORM UPDATE-NOTICE-FILE.

      * CREATE LOAN MEMO RECORD IN THE LM-FILE:
           PERFORM CREATE-LM-FILE-ENTRY.

      * CREATE TRANSACTION RECORD IN THE FRLC->TRJ-FILE:
           PERFORM CREATE-FRTR-FILE.

      * UPDATE DEALER STATISTICS IN THE FROM->DS-FILE
           PERFORM UPDATE-FRDS-FILE.

      * UPDATE TODAYS TOTALS IN THE FR->TY-FILE
           PERFORM UPDATE-FRTY-FILE.

      * CREATE TRANSACTION RECORD IN THE TO->TRJ-FILE:
           PERFORM CREATE-TOTR-FILE.

      * UPDATE DEALER STATISTICS IN THE TO->DS-FILE
           PERFORM UPDATE-TODS-FILE.

      * UPDATE TODAYS TOTALS IN THE TO->TY-FILE
           PERFORM UPDATE-TOTY-FILE.

      * REMOVE WK RECORD FOR THIS ACCOUNT
           PERFORM DELETE-WK-FILE.

      * REDUCE HEADER RECORD WORK FILE:
           MOVE " " TO WK-TYPE.
           MOVE 0 TO WK-ACCTNO.
           PERFORM START-WK-FILE.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
                OR WK-TYPE NOT = " "
              GO TO IO-ERROR.

           SUBTRACT 1 FROM WK-NOSEL.
           SUBTRACT PD-CURBAL FROM WK-AMTSEL.
           PERFORM REWRITE-WK-FILE.

           IF NOT (PRU-LP = "VDU" OR "PRU")
              MOVE "SNNN060NOTFR." TO VDU
              MOVE WK-NOSEL TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN072AMTTFR." TO VDU
              MOVE WK-AMTSEL TO VDUNUM2
              PERFORM SCREENIO.

           IF FIRST-TIME = "Y"
              MOVE "Y" TO RECORDS-FOUND
              MOVE LN-OWNBR TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS.

           MOVE PD-RECORD TO LN-REC.

           PERFORM DETAIL-LINE-PROCESS.

      * TEST FOR FORM REQUEST:
           IF FORMNO NOT = 0
              PERFORM CREATE-FM-REC.

           ADD 1 PD-ACCTNO GIVING WK-ACCTNO.

      * DETERMINE IF AUTO BUILD NEW SPR
      *   THIS ASSUMES THAT THE ORIGINAL LOAN'S SPR RECORD
      *   HAS NOT BEEN OVERRIDEN.
      
           IF CHG-SPR-LC = "Y"
              IF BUILD-SPR = "Y"
                 MOVE TO-LC TO SP-SPRCLASS
                 PERFORM READ-SP1-FILE
                 IF IO-BAD
                    PERFORM WRITE-SP1-FILE
                 END-IF
                 MOVE FR-LC TO SP-SPRCLASS.

           GO TO CLAS-TRANSFER-START.

       CLAS-TRANSFER-ERR-EXIT.
           IF NOT (PRU-LP = "VDU" OR "PRU")
              MOVE " " TO MESS
              MOVE "AN ERROR HAS OCCURED, DURING THE TRANSFER!!"
                                                       TO MESS
              PERFORM SEND-MESS
              MOVE "MAKE NOTE OF THE ACCOUNT NO. !!" TO MESS
              PERFORM SEND-MESS.
       CLAS-TRANSFER-EXIT.
           EXIT.

      *************************************************
       CHANGE-CLASS-LN-FILE SECTION.
      *************************************************
           MOVE " " TO ERR-FG.
           MOVE 0 TO HOLD-FROM-LN-CLASS.

           PERFORM OPEN-LN1-FILE.
           MOVE PD-OWNBR  TO LN-OWNBR.
           MOVE PD-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE LN-CLASS TO HOLD-FROM-LN-CLASS.
           MOVE TO-LC TO LN-CLASS.
           IF CHG-SPR-LC = "Y"
              MOVE TO-LC TO LN-SPRCLASS.
           IF TO-DLNO NOT = 0
              MOVE TO-DLNO TO LN-DLNO.
           PERFORM REWRITE-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

       CHANGE-CLASS-LN-FILE-EXIT.
           EXIT.

      *************************************************
       CREATE-LM-FILE-ENTRY SECTION.
      *************************************************
           MOVE " " TO ERR-FG.

           PERFORM OPEN-LM1-FILE.

           MOVE PD-ACCTNO TO LM-ACCTNO.
           SUBTRACT TRANS-DATE FROM 99999999 GIVING LM-DATE.
           MOVE 0 TO LM-SEQ.

       CREATE-LM-FILE-ENTRY-NEXT.
           IF LM-SEQ = 999
              GO TO CREATE-LM-FILE-ENTRY-EXIT.

           ADD 1 TO LM-SEQ.
           PERFORM READ-LM1-FILE.
           IF IO-GOOD
      *       UNLOCK LM-FILE
              GO TO CREATE-LM-FILE-ENTRY-NEXT.

           MOVE PD-OWNBR TO LM-BRNO.
           MOVE "9"      TO LM-MEMO-TYPE.

           MOVE SPACES TO MESS.
           MOVE PD-CURBAL TO NUM-EDIT.
           STRING
           "TFR TO CL " TO-LC " FROM CL " HOLD-FROM-LN-CLASS", FOR $"
                    NUM-EDIT MESS DELIMITED BY SIZE INTO LM-MEMO.

           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              PERFORM IO-ERROR.

       CREATE-LM-FILE-ENTRY-EXIT.
           PERFORM CLOSE-LM1-FILE.

      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
           MOVE PD-OWNBR  TO NO-BRNO.
           MOVE PD-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           MOVE TRANS-DATE     TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2"    TO FORM-NAM.
      * NEED TO USE PD-RECORD IN HERE
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS PD-RECORD.
           CANCEL FORM-PROGX.

           MOVE PD-OWNBR  TO NO-BRNO.
           MOVE PD-ACCTNO TO NO-ACCTNO.
           IF IO-FG = 0
              PERFORM REWRITE-NO1-FILE
           ELSE
              PERFORM WRITE-NO1-FILE.

      *************************************************
       CREATE-FRTR-FILE SECTION.
      *************************************************
           MOVE " " TO ERR-FG.

           PERFORM OPEN-TRJ1-FILE.

           MOVE EXT-CONAME-USER-LOC TO TJ-BRNO.
           MOVE TRANS-DATE          TO TJ-DATE.
           MOVE PD-CLASS            TO TJ-CLASS.
           MOVE PD-ACCTNO           TO TJ-ACCTNO.
           MOVE ZEROS               TO TJ-LAWCODE. 
           MOVE 0                   TO HOLD-TJ-LINE.
       CREATE-FRTR-FILE-EXISTS.
           ADD 1             TO HOLD-TJ-LINE.
           MOVE HOLD-TJ-LINE TO TJ-LINE.

           MOVE TRJ1-KEY TO LPWSTRJ-TRJ1-KEY.
           PERFORM READ-TRJ1-FILE.
           IF IO-GOOD
      *       UNLOCK TRJ-FILE
              GO TO CREATE-FRTR-FILE-EXISTS.

           PERFORM CLEAR-TRJ-FILE.

           MOVE LPWSTRJ-TRJ1-KEY TO TRJ1-KEY.

           MOVE FR-LC TO REF-FRLC TJ-FRLC.
           MOVE "T"   TO REF-X.
           MOVE TO-LC TO REF-TOLC TJ-TOLC.
           MOVE REF   TO TJ-REFNO.

           MOVE PD-LOANTYPE TO TJ-LOANTYPE.
           MOVE PD-SERVACCT TO TJ-SERVACCT.
           MOVE PD-LOANDATE TO TJ-LOANDATE.
           MOVE PD-PURCD    TO TJ-PURCD.
           MOVE PD-SRCD     TO TJ-SRCD.
           MOVE PD-COLLCD   TO TJ-COLLCD.
           MOVE PD-ORGTERM  TO TJ-ORGTERM.
           MOVE PD-DLRECOURSE TO TJ-DLRECOURSE.
           MOVE PD-INSURED  TO TJ-INSURED.
           MOVE PD-DLNO     TO TJ-DLNO.

           MOVE PD-PLCD   TO TJ-PLCD.
           MOVE PD-PLTYPE TO TJ-PLTYPE.
           MOVE PD-REPOCD TO TJ-REPOCD.

           COMPUTE TJ-LNAMT = PD-SUM-LNAMT * -1.
           COMPUTE TJ-FINCHG = PD-SUM-FINCHG * -1.
           COMPUTE TJ-DLRVPRIN(1) = PD-DLRVPRIN(1) * -1.
           COMPUTE TJ-DLRVPRIN(2) = PD-DLRVPRIN(2) * -1.
           COMPUTE TJ-DLRVPRIN(3) = PD-DLRVPRIN(3) * -1.
           COMPUTE TJ-DLRVCHGS(1) = PD-DLRVCHGS(1) * -1.
           COMPUTE TJ-DLRVCHGS(2) = PD-DLRVCHGS(2) * -1.
           COMPUTE TJ-DLRVCHGS(3) = PD-DLRVCHGS(3) * -1.
           COMPUTE TJ-DLPARVPAID = PD-DLPARVPAID * -1.
           COMPUTE TJ-DLPARVHELD = PD-DLPARVHELD * -1.
           COMPUTE TJ-DLDISC = PD-DLDISC * -1.
           COMPUTE TJ-PROCEEDS = PD-PROCEEDS * -1.
           COMPUTE TJ-DLPARTRCAMT = PD-DLPARTRCAMT * -1.
           COMPUTE TJ-PURBAL = PD-CURBAL * -1.
           COMPUTE TJ-MAINTFEE = PD-SUM-MAINTFEE * -1.
           COMPUTE TJ-SERCHG = PD-SUM-SERCHG * -1.
           COMPUTE TJ-EXTCHG = PD-SUM-EXTCHG * -1.
           COMPUTE TJ-INTCHG = PD-SUM-INTCHG * -1.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF SUB NOT > 4
                 MOVE PD-SSNO(SUB) TO TJ-SSNO(SUB)
                 MOVE PD-MAKERCD(SUB) TO TJ-MAKERCD(SUB)
              END-IF
           END-PERFORM.

      * SET UNEARNED AMOUNTS INTO TJ-WORKAMT'S:
           IF PD-SERVACCT = "N"
              MULTIPLY -1 BY SAVE-UNEARN(1) GIVING TJ-UNEARN(1)
              MULTIPLY -1 BY SAVE-UNEARN(2) GIVING TJ-UNEARN(2)
              MULTIPLY -1 BY SAVE-UNEARN(3) GIVING TJ-UNEARN(3)
              MULTIPLY -1 BY SAVE-UNEARN(4) GIVING TJ-UNEARN(4)
              MULTIPLY -1 BY SAVE-UNEARN(5) GIVING TJ-UNEARN(5)
              MULTIPLY -1 BY SAVE-UNEARN(6) GIVING TJ-UNEARN(6).

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO TJ-USERID.
           MOVE POST-TIME TO TJ-POSTTIME.
           MOVE POST-DATE TO TJ-POSTDATE.

           PERFORM WRITE-TRJ1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-TRJ1-FILE.

       CREATE-FRTR-FILE-EXIT.
           EXIT.

      *************************************************
       UPDATE-FRDS-FILE SECTION.
      *************************************************
           MOVE " " TO ERR-FG.

           PERFORM OPEN-DS1-FILE.
           MOVE "N" TO REC-EXISTS.
       UPDATE-FRDS-FILE-EXISTS.
           PERFORM SETUP-DS-DATE-CCYYMM.
           MOVE PD-OWNBR TO DS-BRANCH.
           MOVE PD-DLNO  TO DS-DLNO.
           MOVE PD-CLASS TO DS-CLASS.
           PERFORM READ-DS1-FILE.
           IF NOT IO-GOOD
              IF REC-EXISTS = "Y"
                 GO TO IO-ERROR
              ELSE
                 PERFORM DS1-FR-LC-CREATE-REC
                 GO TO UPDATE-FRDS-FILE-EXISTS.

      * REPO AND P&L INCLUDED IN DS-JDOSUNIT:
           IF PD-CURBAL NOT = 0
              IF PD-JDDATE NOT = 0
                 SUBTRACT PD-CURBAL FROM DS-JDOSAMT(DSCUR)
                 SUBTRACT 1 FROM DS-JDOSUNIT(DSCUR).

      * TEST FOR P&L,
      * P&L 
      *  IF PL, SUBTRACT FROM PL OUTSTANDING;  IF PL & REPOED
      *         AFTER PL, ALSO SUBTRACT FROM REPO SLOTS 4 & 5
      
           IF (PD-CURBAL NOT = 0)
              IF NOT (PD-PLCD = " " AND PD-PLDATE = 0)
                 SUBTRACT PD-CURBAL FROM DS-PLOSAMT(DSCUR)
                 SUBTRACT 1 FROM DS-PLOSUNIT(DSCUR)
      * ONHAND OR SOLD REPO IN P&L RECEIVABLES:
                 IF PD-REPOCD = "X" OR "S"
                    IF PD-REPOCD = "X"
                      SUBTRACT PD-CURBAL FROM DS-AMT-REPO(DSCUR DSNET 4)
                      SUBTRACT 1 FROM DS-NO-REPO(DSCUR DSNET 4)
                    ELSE
                      SUBTRACT PD-CURBAL FROM DS-AMT-REPO(DSCUR DSNET 5)
                      SUBTRACT 1 FROM DS-NO-REPO(DSCUR DSNET 5)
                    END-IF
                 END-IF
              ELSE
      * NOT P&L:
      *   UPDATE DEALER WEIGHTED AVERAGE STATISTICS
                 COMPUTE DS-SUMWGTTM = DS-SUMWGTTM + (PD-ORGTERM * -1)
                 COMPUTE DS-SUMWGTAV = DS-SUMWGTAV + (PD-LNAMT * -1)
                 COMPUTE DS-SUMWGTTA = DS-SUMWGTTA +
                                       (PD-ORGTERM * PD-LNAMT * -1)
      * UPDATE DEALER RECOURSE STATISTICS
                 PERFORM GET-RECOURSE-SUB
                 IF SUB NOT = 0
                    SUBTRACT 1 FROM DS-RCOURSE-UNITS(SUB)
                    MOVE PD-CURBAL TO PART-RC-AMT
                    IF SUB = 2
                       IF PD-CURBAL > PD-DLPARTRCAMT
                          MOVE PD-DLPARTRCAMT TO PART-RC-AMT
                       END-IF
                    END-IF
                    SUBTRACT PART-RC-AMT FROM DS-RCOURSE-AMT(SUB)
                 END-IF
      * TEST FOR ON HAND REPO:
                 IF LN-REPOCD = "X"
                    SUBTRACT PD-CURBAL FROM DS-AMT-REPO(DSCUR DSNET 1)
                    SUBTRACT 1 FROM DS-NO-REPO(DSCUR DSNET 1)
                 ELSE
      * TEST FOR SOLD REPO:
                 IF LN-REPOCD = "S"
                    SUBTRACT PD-CURBAL FROM DS-AMT-REPO(DSCUR DSNET 3)
                    SUBTRACT 1 FROM DS-NO-REPO(DSCUR DSNET 3)
                 ELSE
      * NON REPO:
                    PERFORM UPDATE-DS1-FR-LC-DELINQ
                    SUBTRACT PD-CURBAL FROM DS-OUTSAMT(1)
                    SUBTRACT 1 FROM DS-OUTSUNIT(1).

      * UPDATE UNEARNED STATISTICS NET
      
           IF GP-UNEARNED-STATS = "Y"
              SUBTRACT SAVE-UNEARN(1) FROM DS-UNEARNED-AMT(1 1 1)
              SUBTRACT SAVE-UNEARN(2) FROM DS-UNEARNED-AMT(1 1 2)
              SUBTRACT SAVE-UNEARN(3) FROM DS-UNEARNED-AMT(1 1 3)
              SUBTRACT SAVE-UNEARN(5) FROM DS-UNEARNED-AMT(1 1 5)
              IF NOT( GP-INCL-OTH-IN-EARNED-N OR
                                           GP-INCL-OTH-IN-EARNED-R)
                 SUBTRACT SAVE-UNEARN(6) FROM DS-UNEARNED-AMT(1 1 6).

              IF NOT (GP-INCL-OTH-IN-EARNED-R)
                 SUBTRACT SAVE-UNEARN(4) FROM DS-UNEARNED-AMT(1 1 4).

           PERFORM REWRITE-DS1-FILE.
           PERFORM CLOSE-DS1-FILE.

       UPDATE-FRDS-FILE-EXIT.
           EXIT.

      *********************************************
       DS1-FR-LC-CREATE-REC SECTION.
      *********************************************
           MOVE "Y" TO REC-EXISTS.
           MOVE " " TO DS-REC.

           PERFORM CLEAR-DS-FILE.

           PERFORM SETUP-DS-DATE-CCYYMM.
           MOVE PD-OWNBR TO DS-BRANCH.
           MOVE PD-DLNO TO DS-DLNO.
           MOVE PD-CLASS TO DS-CLASS.

           PERFORM WRITE-DS1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.


      *************************************************
       UPDATE-FRTY-FILE SECTION.
      *************************************************
           MOVE " " TO ERR-FG.

           PERFORM OPEN-TY1-FILE.
           MOVE "N" TO REC-EXISTS.
       UPDATE-FRTY-FILE-EXISTS.
           MOVE EXT-CONAME-USER-LOC TO TY-BRNO.
           PERFORM SET-FRTY-KEY.
           PERFORM READ-TY1-FILE.
           IF IO-GOOD
              GO TO UPDATE-FRTY-FILE-REC.
           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           PERFORM CLEAR-TY-FILE.
           MOVE EXT-CONAME-USER-LOC TO TY-BRNO.
           PERFORM SET-FRTY-KEY.
           PERFORM WRITE-TY1-FILE.
           IF IO-BAD AND E-CODE NOT = "22"
              GO TO IO-ERROR.
           GO TO UPDATE-FRTY-FILE-EXISTS.

       UPDATE-FRTY-FILE-REC.
           PERFORM GET-SRCD-SUB.

           IF (PD-CURBAL NOT = 0)
              IF SRCD-NOT-PL
      * NOT P&L:
                 COMPUTE TY-SUMWGTTM = TY-SUMWGTTM + (PD-ORGTERM * -1)
                 COMPUTE TY-SUMWGTAV = TY-SUMWGTAV + (PD-LNAMT * -1)
                 COMPUTE TY-SUMWGTTA = TY-SUMWGTTA +
                                          (PD-ORGTERM * PD-LNAMT * -1)
      * ONHAND REPO:
                 IF LN-REPOCD = "X"
                    SUBTRACT 1 FROM TY-NO-REPO(TYNET 1)
                    SUBTRACT PD-CURBAL FROM TY-AMT-REPO(TYNET 1)
                    SUBTRACT PD-OT2BAL FROM TY-O2-REPO(TYNET 1)
                    SUBTRACT PD-INTCHG FROM TY-INT-REPO(TYNET 1)
                    SUBTRACT PD-EXTCHG FROM TY-INT-REPO(TYNET 1)
                 ELSE
      * SOLD REPO:
                 IF LN-REPOCD = "S"
                    SUBTRACT 1 FROM TY-NO-REPO(TYNET 3)
                    SUBTRACT PD-CURBAL FROM TY-AMT-REPO(TYNET 3)
                    SUBTRACT PD-OT2BAL FROM TY-O2-REPO(TYNET 3)
                    SUBTRACT PD-INTCHG FROM TY-INT-REPO(TYNET 3)
                    SUBTRACT PD-EXTCHG FROM TY-INT-REPO(TYNET 3)
                 ELSE
      * NON REPO:
                    PERFORM UPDATE-TY1-FR-LC-DELINQ
                    SUBTRACT 1 FROM TY-NO(TYNET SRCD)
                    SUBTRACT PD-CURBAL FROM TY-AMT(TYNET SRCD)
                    SUBTRACT PD-OT2BAL FROM TY-O2(TYNET SRCD)
                    SUBTRACT PD-INTCHG FROM TY-INT(TYNET SRCD)
                    SUBTRACT PD-EXTCHG FROM TY-INT(TYNET SRCD)
              ELSE
      * P&L (ACTIVE/INACTIVE):
                 SUBTRACT 1 FROM TY-NO-PL(TYNET SRCD)
                 SUBTRACT PD-CURBAL FROM TY-AMT-PL(TYNET SRCD)
                 SUBTRACT PD-OT2BAL FROM TY-O2-PL(TYNET SRCD)
                 SUBTRACT PD-INTCHG FROM TY-INT-PL(TYNET SRCD)
                 SUBTRACT PD-EXTCHG FROM TY-INT-PL(TYNET SRCD)
      * ONHAND OR SOLD REPO IN P&L RECEIVABLES:
                 IF PD-REPOCD = "X" OR "S"
                    IF PD-REPOCD = "X"
                       SUBTRACT 1 FROM TY-NO-REPO(TYNET 4)
                       SUBTRACT PD-CURBAL FROM TY-AMT-REPO(TYNET 4)
                       SUBTRACT PD-OT2BAL FROM TY-O2-REPO(TYNET 4)
                       SUBTRACT PD-INTCHG FROM TY-INT-REPO(TYNET 4)
                       SUBTRACT PD-EXTCHG FROM TY-INT-REPO(TYNET 4)
                    ELSE
                       SUBTRACT 1 FROM TY-NO-REPO(TYNET 5)
                       SUBTRACT PD-CURBAL FROM TY-AMT-REPO(TYNET 5)
                       SUBTRACT PD-OT2BAL FROM TY-O2-REPO(TYNET 5)
                       SUBTRACT PD-INTCHG FROM TY-INT-REPO(TYNET 5)
                       SUBTRACT PD-EXTCHG FROM TY-INT-REPO(TYNET 5).

      * NOTE:    ACTIVE & REPO & P&L DUAL LOGIC FOLLOWS
      
      * UPDATE LOANS SOLD/TRANSFERRED TO ANOTHER BRANCH.
      
           ADD 1 TO TY-NO-SOLD.
           ADD PD-CURBAL TO TY-AMT-SOLD.

      * UPDATE UNEARNED STATISTICS NET
      
           IF GP-UNEARNED-STATS = "Y"
              SUBTRACT SAVE-UNEARN(1) FROM TY-UNEARNED-AMT(1 1)
              SUBTRACT SAVE-UNEARN(2) FROM TY-UNEARNED-AMT(1 2)
              SUBTRACT SAVE-UNEARN(3) FROM TY-UNEARNED-AMT(1 3)
              SUBTRACT SAVE-UNEARN(5) FROM TY-UNEARNED-AMT(1 5)
              IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                                       GP-INCL-OTH-IN-EARNED-R)
                 SUBTRACT SAVE-UNEARN(6) FROM TY-UNEARNED-AMT(1 6).

              IF NOT (GP-INCL-OTH-IN-EARNED-R)
                 SUBTRACT SAVE-UNEARN(4) FROM TY-UNEARNED-AMT(1 4).

           PERFORM REWRITE-TY1-FILE.
           PERFORM CLOSE-TY1-FILE.

       UPDATE-FRTY-FILE-EXIT.
           EXIT.

      *************************************************
       CREATE-TOTR-FILE SECTION.
      *************************************************
           MOVE " " TO ERR-FG.

           PERFORM OPEN-TRJ1-FILE.

           MOVE EXT-CONAME-USER-LOC TO TJ-BRNO.
           MOVE TRANS-DATE          TO TJ-DATE.
           MOVE TO-LC               TO TJ-CLASS.
           MOVE PD-ACCTNO           TO TJ-ACCTNO.
           MOVE 0                   TO TJ-LAWCODE.
           MOVE 0                   TO HOLD-TJ-LINE.
       CREATE-TOTR-FILE-EXISTS.
           ADD 1             TO HOLD-TJ-LINE.
           MOVE HOLD-TJ-LINE TO TJ-LINE.

           MOVE TRJ1-KEY TO LPWSTRJ-TRJ1-KEY.
           PERFORM READ-TRJ1-FILE.
           IF IO-GOOD
      *       UNLOCK TRJ-FILE
              GO TO CREATE-TOTR-FILE-EXISTS.

           PERFORM CLEAR-TRJ-FILE.

           MOVE LPWSTRJ-TRJ1-KEY TO TRJ1-KEY.

           MOVE FR-LC TO REF-FRLC.
           MOVE "T"   TO REF-X.
           MOVE TO-LC TO REF-TOLC.
           MOVE REF   TO TJ-REFNO.

           MOVE PD-LOANTYPE TO TJ-LOANTYPE.
           MOVE PD-SERVACCT TO TJ-SERVACCT.
           MOVE PD-LOANDATE TO TJ-LOANDATE.
           MOVE PD-PURCD    TO TJ-PURCD.
           MOVE PD-SRCD     TO TJ-SRCD.
           MOVE PD-COLLCD   TO TJ-COLLCD.
           MOVE PD-ORGTERM  TO TJ-ORGTERM.
           MOVE PD-DLRECOURSE TO TJ-DLRECOURSE.
           MOVE PD-INSURED  TO TJ-INSURED.
           IF TO-DLNO = 0
              MOVE PD-DLNO  TO TJ-DLNO
           ELSE
              MOVE TO-DLNO  TO TJ-DLNO.

           MOVE PD-PLCD   TO TJ-PLCD.
           MOVE PD-PLTYPE TO TJ-PLTYPE.
           MOVE PD-REPOCD TO TJ-REPOCD.

           COMPUTE TJ-LNAMT = PD-SUM-LNAMT * 1.
           COMPUTE TJ-FINCHG = PD-SUM-FINCHG * 1.
           COMPUTE TJ-DLRVPRIN(1) = PD-DLRVPRIN(1) * -1.
           COMPUTE TJ-DLRVPRIN(2) = PD-DLRVPRIN(2) * -1.
           COMPUTE TJ-DLRVPRIN(3) = PD-DLRVPRIN(3) * -1.
           COMPUTE TJ-DLRVCHGS(1) = PD-DLRVCHGS(1) * -1.
           COMPUTE TJ-DLRVCHGS(2) = PD-DLRVCHGS(2) * -1.
           COMPUTE TJ-DLRVCHGS(3) = PD-DLRVCHGS(3) * -1.
           COMPUTE TJ-DLPARVPAID = PD-DLPARVPAID * -1.
           COMPUTE TJ-DLPARVHELD = PD-DLPARVHELD * -1.
           COMPUTE TJ-DLDISC = PD-DLDISC * 1.
           COMPUTE TJ-PROCEEDS = PD-PROCEEDS * 1.
           COMPUTE TJ-DLPARTRCAMT = PD-DLPARTRCAMT * 1.
           COMPUTE TJ-PURBAL = PD-CURBAL * 1.
           COMPUTE TJ-MAINTFEE = PD-SUM-MAINTFEE * 1.
           COMPUTE TJ-SERCHG = PD-SUM-SERCHG * 1.
           COMPUTE TJ-EXTCHG = PD-SUM-EXTCHG * 1.
           COMPUTE TJ-INTCHG = PD-SUM-INTCHG * 1.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF SUB NOT > 4
                 MOVE PD-SSNO(SUB) TO TJ-SSNO(SUB)
                 MOVE PD-MAKERCD(SUB) TO TJ-MAKERCD(SUB)
              END-IF
           END-PERFORM.

      * SET UNEARNED AMOUNTS INTO TJ-WORKAMT'S:
           IF PD-SERVACCT = "N"
              MULTIPLY 1 BY SAVE-UNEARN(1) GIVING TJ-UNEARN(1)
              MULTIPLY 1 BY SAVE-UNEARN(2) GIVING TJ-UNEARN(2)
              MULTIPLY 1 BY SAVE-UNEARN(3) GIVING TJ-UNEARN(3)
              MULTIPLY 1 BY SAVE-UNEARN(4) GIVING TJ-UNEARN(4)
              MULTIPLY 1 BY SAVE-UNEARN(5) GIVING TJ-UNEARN(5)
              MULTIPLY 1 BY SAVE-UNEARN(6) GIVING TJ-UNEARN(6).

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO TJ-USERID.
           MOVE POST-TIME TO TJ-POSTTIME.
           MOVE POST-DATE TO TJ-POSTDATE.

           PERFORM WRITE-TRJ1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-TRJ1-FILE.

       CREATE-TOTR-FILE-EXIT.
           EXIT.

      *************************************************
       UPDATE-TODS-FILE SECTION.
      *************************************************
           MOVE " " TO ERR-FG.

           PERFORM OPEN-DS1-FILE.
           MOVE "N" TO REC-EXISTS.
       UPDATE-TODS-FILE-EXISTS.
           PERFORM SETUP-DS-DATE-CCYYMM.
           MOVE EXT-CONAME-USER-LOC TO DS-BRANCH.
           IF TO-DLNO = 0
              MOVE PD-DLNO TO DS-DLNO
           ELSE
              MOVE TO-DLNO TO DS-DLNO.
           MOVE TO-LC TO DS-CLASS.
           PERFORM READ-DS1-FILE.
           IF NOT IO-GOOD
              IF REC-EXISTS = "Y"
                 GO TO IO-ERROR
              ELSE
                 PERFORM DS1-TO-LC-CREATE-REC
                 GO TO UPDATE-TODS-FILE-EXISTS.

      * REPO AND P&L INCLUDED IN DS-JDOSUNIT:
           IF PD-CURBAL NOT = 0
              IF PD-JDDATE NOT = 0
                 ADD PD-CURBAL TO DS-JDOSAMT(DSCUR)
                 ADD 1 TO DS-JDOSUNIT(DSCUR).

      * TEST FOR P&L,
      *  P&L 
      *   IF PL, ADD TO PL OUTSTANDING;  IF PL & REPOED
      *   AFTER PL, ALSO ADD TO REPO SLOTS 4 & 5
      
           IF (PD-CURBAL NOT = 0)
              IF NOT (PD-PLCD = " " AND PD-PLDATE = 0)
                 ADD PD-CURBAL TO DS-PLOSAMT(DSCUR)
                 ADD 1 TO DS-PLOSUNIT(DSCUR)
      * ONHAND OR SOLD REPO IN P&L RECEIVABLES:
                 IF PD-REPOCD = "X" OR "S"
                    IF PD-REPOCD = "X"
                       ADD PD-CURBAL TO DS-AMT-REPO(DSCUR DSNET 4)
                       ADD 1 TO DS-NO-REPO(DSCUR DSNET 4)
                    ELSE
                       ADD PD-CURBAL TO DS-AMT-REPO(DSCUR DSNET 5)
                       ADD 1 TO DS-NO-REPO(DSCUR DSNET 5)
                    END-IF
                 END-IF
              ELSE
      * NOT P&L:
      *   UPDATE DEALER WEIGHTED AVERAGE STATISTICS
                 COMPUTE DS-SUMWGTTM = DS-SUMWGTTM + (PD-ORGTERM * 1)
                 COMPUTE DS-SUMWGTAV = DS-SUMWGTAV + (PD-LNAMT * 1)
                 COMPUTE DS-SUMWGTTA = DS-SUMWGTTA +
                                       (PD-ORGTERM * PD-LNAMT * 1)
      * UPDATE DEALER RECOURSE STATISTICS
                 PERFORM GET-RECOURSE-SUB
                 IF SUB NOT = 0
                    ADD 1 TO DS-RCOURSE-UNITS(SUB)
                    MOVE PD-CURBAL TO PART-RC-AMT
                    IF SUB = 2
                       IF PD-CURBAL > PD-DLPARTRCAMT
                          MOVE PD-DLPARTRCAMT TO PART-RC-AMT
                       END-IF
                    END-IF
                    ADD PART-RC-AMT TO DS-RCOURSE-AMT(SUB)
                 END-IF
      * TEST FOR ON HAND REPO:
                 IF LN-REPOCD = "X"
                    ADD PD-CURBAL TO DS-AMT-REPO(DSCUR DSNET 1)
                    ADD 1 TO DS-NO-REPO(DSCUR DSNET 1)
                 ELSE
      * TEST FOR SOLD REPO:
                 IF LN-REPOCD = "S"
                    ADD PD-CURBAL TO DS-AMT-REPO(DSCUR DSNET 3)
                    ADD 1 TO DS-NO-REPO(DSCUR DSNET 3)
                 ELSE
      * NON REPO:
                    PERFORM UPDATE-DS1-TO-LC-DELINQ
                    ADD PD-CURBAL TO DS-OUTSAMT(1)
                    ADD 1 TO DS-OUTSUNIT(1).

      * UPDATE UNEARNED STATISTICS NET
      
           IF GP-UNEARNED-STATS = "Y"
              ADD SAVE-UNEARN(1) TO DS-UNEARNED-AMT(1 1 1)
              ADD SAVE-UNEARN(2) TO DS-UNEARNED-AMT(1 1 2)
              ADD SAVE-UNEARN(3) TO DS-UNEARNED-AMT(1 1 3)
              ADD SAVE-UNEARN(5) TO DS-UNEARNED-AMT(1 1 5)
              IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                                 GP-INCL-OTH-IN-EARNED-R)
                 ADD SAVE-UNEARN(6) TO DS-UNEARNED-AMT(1 1 6).

              IF NOT (GP-INCL-OTH-IN-EARNED-R)
                 ADD SAVE-UNEARN(4) TO DS-UNEARNED-AMT(1 1 4).

           PERFORM REWRITE-DS1-FILE.
           PERFORM CLOSE-DS1-FILE.

       UPDATE-TODS-FILE-EXIT.
           EXIT.

      ******************************************
       DS1-TO-LC-CREATE-REC SECTION.
      ******************************************
           MOVE "Y" TO REC-EXISTS.
           MOVE " " TO DS-REC.

           PERFORM CLEAR-DS-FILE.
           PERFORM SETUP-DS-DATE-CCYYMM.
           MOVE EXT-CONAME-USER-LOC TO DS-BRANCH.
           IF TO-DLNO = 0
              MOVE PD-DLNO TO DS-DLNO
           ELSE
              MOVE TO-DLNO TO DS-DLNO.
           MOVE TO-LC TO DS-CLASS.

           PERFORM WRITE-DS1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.


      *************************************************
       UPDATE-TOTY-FILE SECTION.
      *************************************************
           MOVE " " TO ERR-FG.

           PERFORM OPEN-TY1-FILE.
           MOVE "N" TO REC-EXISTS.
       UPDATE-TOTY-FILE-EXISTS.
           MOVE EXT-CONAME-USER-LOC TO TY-BRNO.
           PERFORM SET-TOTY-KEY.
           PERFORM READ-TY1-FILE.
           IF IO-GOOD
              GO TO UPDATE-TOTY-FILE-REC.
           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           PERFORM CLEAR-TY-FILE.
           MOVE EXT-CONAME-USER-LOC TO TY-BRNO.
           PERFORM SET-TOTY-KEY.
           PERFORM WRITE-TY1-FILE.
           IF IO-BAD AND E-CODE NOT = "22"
              GO TO IO-ERROR.
           GO TO UPDATE-TOTY-FILE-EXISTS.

       UPDATE-TOTY-FILE-REC.
           PERFORM GET-SRCD-SUB.

           IF (PD-CURBAL NOT = 0)
              IF SRCD-NOT-PL
      * NOT P&L:
                 COMPUTE TY-SUMWGTTM = TY-SUMWGTTM + (PD-ORGTERM * 1)
                 COMPUTE TY-SUMWGTAV = TY-SUMWGTAV + (PD-LNAMT * 1)
                 COMPUTE TY-SUMWGTTA = TY-SUMWGTTA +
                                          (PD-ORGTERM * PD-LNAMT * 1)
      * ONHAND REPO:
                 IF LN-REPOCD = "X"
                    ADD 1 TO TY-NO-REPO(TYNET 1)
                    ADD PD-CURBAL TO TY-AMT-REPO(TYNET 1)
                    ADD PD-OT2BAL TO TY-O2-REPO(TYNET 1)
                    ADD PD-INTCHG TO TY-INT-REPO(TYNET 1)
                    ADD PD-EXTCHG TO TY-INT-REPO(TYNET 1)
                 ELSE
      * SOLD REPO:
                 IF LN-REPOCD = "S"
                    ADD 1 TO TY-NO-REPO(TYNET 3)
                    ADD PD-CURBAL TO TY-AMT-REPO(TYNET 3)
                    ADD PD-OT2BAL TO TY-O2-REPO(TYNET 3)
                    ADD PD-INTCHG TO TY-INT-REPO(TYNET 3)
                    ADD PD-EXTCHG TO TY-INT-REPO(TYNET 3)
                 ELSE
      * NON REPO:
                    PERFORM UPDATE-TY1-TO-LC-DELINQ
                    ADD 1 TO TY-NO(TYNET SRCD)
                    ADD PD-CURBAL TO TY-AMT(TYNET SRCD)
                    ADD PD-OT2BAL TO TY-O2(TYNET SRCD)
                    ADD PD-INTCHG TO TY-INT(TYNET SRCD)
                    ADD PD-EXTCHG TO TY-INT(TYNET SRCD)
              ELSE
      * P&L (ACTIVE/INACTIVE):
                 ADD 1 TO TY-NO-PL(TYNET SRCD)
                 ADD PD-CURBAL TO TY-AMT-PL(TYNET SRCD)
                 ADD PD-OT2BAL TO TY-O2-PL(TYNET SRCD)
                 ADD PD-INTCHG TO TY-INT-PL(TYNET SRCD)
                 ADD PD-EXTCHG TO TY-INT-PL(TYNET SRCD)
      * ONHAND OR SOLD REPO IN P&L RECEIVABLES:
                 IF PD-REPOCD = "X" OR "S"
                    IF PD-REPOCD = "X"
                       ADD 1 TO TY-NO-REPO(TYNET 4)
                       ADD PD-CURBAL TO TY-AMT-REPO(TYNET 4)
                       ADD PD-OT2BAL TO TY-O2-REPO(TYNET 4)
                       ADD PD-INTCHG TO TY-INT-REPO(TYNET 4)
                       ADD PD-EXTCHG TO TY-INT-REPO(TYNET 4)
                 ELSE
                       ADD 1 TO TY-NO-REPO(TYNET 5)
                       ADD PD-CURBAL TO TY-AMT-REPO(TYNET 5)
                       ADD PD-OT2BAL TO TY-O2-REPO(TYNET 5)
                       ADD PD-INTCHG TO TY-INT-REPO(TYNET 5)
                       ADD PD-EXTCHG TO TY-INT-REPO(TYNET 5).

      * NOTE:    ACTIVE & REPO & P&L DUAL LOGIC FOLLOWS

      * UPDATE LOANS TRANSFERRED FROM ANOTHER BRANCH.
      
           ADD 1 TO TY-NO-TRANS.
           ADD PD-CURBAL TO TY-AMT-TRANS.

      * UPDATE UNEARNED STATISTICS NET
      
           IF GP-UNEARNED-STATS = "Y"
              ADD SAVE-UNEARN(1) TO TY-UNEARNED-AMT(1 1)
              ADD SAVE-UNEARN(2) TO TY-UNEARNED-AMT(1 2)
              ADD SAVE-UNEARN(3) TO TY-UNEARNED-AMT(1 3)
              ADD SAVE-UNEARN(5) TO TY-UNEARNED-AMT(1 5)
              IF NOT( GP-INCL-OTH-IN-EARNED-N OR
                                    GP-INCL-OTH-IN-EARNED-R)
                 ADD SAVE-UNEARN(6) TO TY-UNEARNED-AMT(1 6).

              IF NOT (GP-INCL-OTH-IN-EARNED-R)
                 ADD SAVE-UNEARN(4) TO TY-UNEARNED-AMT(1 4).

           PERFORM REWRITE-TY1-FILE.
           PERFORM CLOSE-TY1-FILE.

       UPDATE-TOTY-FILE-EXIT.
           EXIT.

      ******************************************************
      *    ESTABLISH CONTRACTUAL AND RECENCY
      *    FOR UPDATE TY1  AND  DS1
      ******************************************************
       ESTABLISH-ORIG-TY-DS-NUMBERS SECTION.
           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.
           PERFORM COMPUTE-PAID-AHEAD.

           MOVE CSUB TO SAVE-CSUB.
           MOVE RDSUB TO SAVE-RDSUB.
           MOVE PSUB TO SAVE-PSUB.

           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE LN-CURBAL TO SAVE-OUTBAL
           ELSE
              SUBTRACT LN-OT2BAL FROM LN-CURBAL GIVING SAVE-OUTBAL.

      ******************************************************
      *    UPDATE DELINQUENCY FROM->TYFILE
      * NOTE: DON'T ALTER SRCD IN THIS ROUTINE
      ******************************************************
       UPDATE-TY1-FR-LC-DELINQ SECTION.
           IF SAVE-OUTBAL NOT = 0
              SUBTRACT 1 FROM TY-UNITS(TYREC SAVE-RDSUB)
                              TY-UNITS(TYCON SAVE-CSUB)
              SUBTRACT SAVE-OUTBAL
                         FROM TY-OUTBAL(TYREC SAVE-RDSUB)
                              TY-OUTBAL(TYCON SAVE-CSUB)
              IF LN-BNKRPTDATE NOT = 0
                 SUBTRACT 1 FROM TY-UNITS-BNKRPT(TYREC SAVE-RDSUB)
                                 TY-UNITS-BNKRPT(TYCON SAVE-CSUB)
                 SUBTRACT SAVE-OUTBAL
                            FROM TY-OUTBAL-BNKRPT(TYREC SAVE-RDSUB)
                                 TY-OUTBAL-BNKRPT(TYCON SAVE-CSUB).

      ******************************************************
      *    UPDATE DELINQUENCY FROM->DSFILE
      * NOTE: DON'T ALTER SRCD IN THIS ROUTINE
      ******************************************************
       UPDATE-DS1-FR-LC-DELINQ SECTION.
           IF SAVE-OUTBAL NOT = 0
              SUBTRACT 1 FROM DS-UNITS(DSNOW DSREC SAVE-RDSUB)
                              DS-UNITS(DSNOW DSCON SAVE-CSUB)
              SUBTRACT SAVE-OUTBAL
                         FROM DS-OUTBAL(DSNOW DSREC SAVE-RDSUB)
                              DS-OUTBAL(DSNOW DSCON SAVE-CSUB)
              IF LN-BNKRPTDATE NOT = 0
                 SUBTRACT 1 FROM DS-UNITS-BNKRPT(DSNOW DSREC SAVE-RDSUB)
                                 DS-UNITS-BNKRPT(DSNOW DSCON SAVE-CSUB)
                 SUBTRACT SAVE-OUTBAL
                          FROM DS-OUTBAL-BNKRPT(DSNOW DSREC SAVE-RDSUB)
                               DS-OUTBAL-BNKRPT(DSNOW DSCON SAVE-CSUB).

      ******************************************************
      *    UPDATE DELINQUENCY TO->TYFILE
      * NOTE: DON'T ALTER SRCD IN THIS ROUTINE
      ******************************************************
       UPDATE-TY1-TO-LC-DELINQ SECTION.
           IF SAVE-OUTBAL NOT = 0
              ADD 1 TO TY-UNITS(TYREC SAVE-RDSUB)
                       TY-UNITS(TYCON SAVE-CSUB)
              ADD SAVE-OUTBAL
                    TO TY-OUTBAL(TYREC SAVE-RDSUB)
                       TY-OUTBAL(TYCON SAVE-CSUB)
              IF LN-BNKRPTDATE NOT = 0
                 ADD 1 TO TY-UNITS-BNKRPT(TYREC SAVE-RDSUB)
                          TY-UNITS-BNKRPT(TYCON SAVE-CSUB)
                 ADD SAVE-OUTBAL
                       TO TY-OUTBAL-BNKRPT(TYREC SAVE-RDSUB)
                          TY-OUTBAL-BNKRPT(TYCON SAVE-CSUB).

      ******************************************************
      *    UPDATE DELINQUENCY TO->DSFILE
      * NOTE: DON'T ALTER SRCD IN THIS ROUTINE
      ******************************************************
       UPDATE-DS1-TO-LC-DELINQ SECTION.
           IF SAVE-OUTBAL NOT = 0
              ADD 1 TO DS-UNITS(DSNOW DSREC SAVE-RDSUB)
                       DS-UNITS(DSNOW DSCON SAVE-CSUB)
              ADD SAVE-OUTBAL TO DS-OUTBAL(DSNOW DSREC SAVE-RDSUB)
                                 DS-OUTBAL(DSNOW DSCON SAVE-CSUB)
              IF LN-BNKRPTDATE NOT = 0
                 ADD 1 TO DS-UNITS-BNKRPT(DSNOW DSREC SAVE-RDSUB)
                          DS-UNITS-BNKRPT(DSNOW DSCON SAVE-CSUB)
                 ADD SAVE-OUTBAL
                       TO DS-OUTBAL-BNKRPT(DSNOW DSREC SAVE-RDSUB)
                          DS-OUTBAL-BNKRPT(DSNOW DSCON SAVE-CSUB).


      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE "N" TO RECORDS-FOUND.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO AGEING-DATE
                                 TRANS-DATE.

      * VERIFY BRANCH IS OPEN TODAY:
           PERFORM VERIFY-BRANCH-IS-OPEN.
           IF BR-OPEN-FG = "N"
              MOVE "PROBLEM BRANCH IS NOT OPEN FOR TODAY!" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.

           MOVE "SNNN020FRLC." TO VDU.
           MOVE FR-LC TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020FRLC2." TO VDU.
           MOVE FR2-LC TO VDUNUM0.
           PERFORM SCREENIO.

      * SET TO CLASS:
           MOVE TO-LC TO TEST-LN-CLASS.
           PERFORM VERIFY-CLASS.
           IF STAT NOT = "00"
              MOVE "PROBLEM GETTING TO CLASS!" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.

           MOVE "SNNN020TOLC." TO VDU.
           MOVE TO-LC TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A180TOLCN." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.


      * SET-WK-FILE:
           MOVE FR-LC     TO CLAS-FRLC.
           MOVE TO-LC     TO CLAS-TOLC.
           PERFORM LOAD-CLAS-FILE.
           MOVE CLAS-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE
           "ACCOUNT SELECTION FOR CLASS TRANSFER HAS NOT BEEN DONE!!"
                                                  TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.

           PERFORM OPEN-WK-FILE.
           MOVE " " TO WK-TYPE.
           MOVE 0 TO WK-ACCTNO.
           PERFORM START-WK-FILE.
           PERFORM READ-WK-FILE-NEXT.
           PERFORM CLOSE-WK-FILE.
           IF IO-BAD
                OR WK-TYPE NOT = " "
              MOVE "HEADER RECORD FOR SELECTION MISSING" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           MOVE WK-TO-DLNO TO TO-DLNO.
           MOVE "SNNN060NOTOBE." TO VDU.
           MOVE WK-NOSEL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN072AMTTOBE." TO VDU.
           MOVE WK-AMTSEL TO VDUNUM2.
           PERFORM SCREENIO.
           IF WK-NOSEL = 0
              MOVE "NO ACCOUNTS WERE FOUND TO BE CLASS TRANSFERRED!"
                                                  TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.

           MOVE WK-PL-ONLY TO PL-ONLY.
           MOVE WK-ONHAND TO ONHAND.
           MOVE WK-SOLD TO SOLD.
           MOVE WK-REDEEM TO REDEEM.
           MOVE WK-CHG-SPR-LC TO CHG-SPR-LC.
           MOVE WK-BUILD-SPR TO BUILD-SPR.
           MOVE WK-ZBAL TO ZBAL.

       SETRP.
           MOVE " " TO ERRCD.
           MOVE "U" TO STAT.

           MOVE CLAST2-QUEUE-POS  TO Q-POS.
           MOVE CLAST2-COPIES-POS TO C-POS.
           MOVE CLAST2-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           PERFORM PRU-IN.
           IF STAT NOT = "00"
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.

           IF NOT (PRU-LP = "VDU" OR "PRU")
              MOVE "S  A310NOTFRP." TO VDU
              MOVE "# REMAINING TO BE TRANSFERRED :" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A310AMTTFRP." TO VDU
              MOVE "$ REMAINING ................  :" TO VDUBUF
              PERFORM SCREENIO
              MOVE "SNNN060NOTFR." TO VDU
              MOVE WK-NOSEL TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN072AMTTFR." TO VDU
              MOVE WK-AMTSEL TO VDUNUM2
              PERFORM SCREENIO.

           PERFORM ENTER-PARAMETERS.
           IF ERRCD NOT = " "
              IF ERRCD = "I"
                 GO TO SETRP
              ELSE
                 GO TO INITIALIZATION-EXIT.

      * MAKE SURE BRANCH HASN'T CLOSED SINCE BEFORE ENTER PARAMETERS
           PERFORM VERIFY-BRANCH-IS-OPEN.
           IF BR-OPEN-FG = "N"
              MOVE "PROBLEM BRANCH IS NOT OPEN FOR TODAY!" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE FR-LC              TO H-FRLC.
           MOVE "-"                TO H-DASH.
           MOVE FR2-LC             TO H-FR2LC.
           MOVE TO-LC              TO H-TOLC.
           MOVE TO-DLNO            TO H-TO-DLNO.
           IF NOT (PRU-LP = "VDU" OR "PRU")
              MOVE "CLASS TRANSFER UPDATE IN PROGRESS........." TO MESS
              PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-SP1-FILE.

           PERFORM ZERO-TT-TOTALS.
           PERFORM ZERO-DD-TOTALS.
           PERFORM ZERO-NM-TOTALS.

       INITIALIZATION-EXIT.
           EXIT.


      ****************************************
       ENTER-PARAMETERS SECTION.
      ****************************************
       ENTER-PARM.
       REC-FORM.
           MOVE "ENNN030FORMNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-PARM-ABORT.
           IF VDUSTATUS = "1<"
              PERFORM FORM-SCAN
              IF IO-BAD
                 GO TO REC-FORM.
           IF VDUSTATUS = "1>" GO TO ENTER-PARM-ABORT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-FORM.
           MOVE VDUNUM0 TO FORMNO FD-FMNO.
           IF FORMNO = 0
              MOVE " " TO FD-DESC
           ELSE
              PERFORM OPEN-FD1-FILE
              PERFORM READ-FD1-FILE
              PERFORM CLOSE-FD1-FILE
              IF NOT IO-GOOD
                 GO TO REC-FORM
              ELSE
                 IF NOT (FD-1STFILE = 1 OR 2 OR 4)
                    MOVE "FIRST FILE MUST BE LN, BW, OR NO FILE"
                                                         TO MESS
                    PERFORM SEND-MESS
                    GO TO REC-FORM.
           MOVE "S  A300FORMALP." TO VDU.
           MOVE FD-DESC TO VDUBUF.
           PERFORM SCREENIO.

       ACCEPT-IT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENTER-PARM-ABORT.
           IF VDUSTATUS = "1U" GO TO REC-FORM.
           IF VDUSTATUS = "10"
              MOVE "I" TO ERRCD
              GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-IT.
           IF NOT (VDUBUF = "Y"  OR "N") GO TO ACCEPT-IT.
           IF VDUBUF = "N"
              GO TO REC-FORM.
           GO TO EXIT-PARM.

       ENTER-PARM-ABORT.
           MOVE "X" TO ERRCD.

       EXIT-PARM.
           EXIT.

      ********************************
       FORM-SCAN SECTION.
      ********************************
           MOVE 0   TO FDSCAN-KEY.
           MOVE "C" TO FDSCAN-FMTYPE.
           MOVE "S" TO FDSCAN-ALLOW-CHG.
           MOVE "WI/FDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH FDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF FDSCAN-STATUS NOT = "00" OR FDSCAN-KEY = " "
              MOVE 9 TO IO-FG
              GO TO EXIT-FORM-SCAN.

           MOVE "SNNN030FORMNO." TO VDU.
           MOVE FDSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE FDSCAN-KEY TO VDUNUM0.

       EXIT-FORM-SCAN.
           EXIT.

      ***************************************
       DETAIL-LINE-PROCESS SECTION.
      ***************************************

           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME.

           MOVE 1 TO BW-SUB.
           PERFORM PROCESS-BORROWER.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO HOLD-LPS-NUM.
           MOVE H-LPS-NUM TO D-LOAN-NUM.
           MOVE LN-1STPYDA TO D-DUEDAY.
           MOVE LN-DLNO TO D-DLNO.
           IF LN-DATE-PAID-LAST NOT = 0
              MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-PAYDATE
           ELSE
              MOVE LN-LOANDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-PAYDATE
              MOVE "." TO D-DASH1 D-DASH2.

           IF RDPOT30 = "Y"
              MOVE "<P>" TO D-RECENCYX
           ELSE
           IF RDPOT30 = "M"
              MOVE "<M>" TO D-RECENCYX
           ELSE
           IF RDPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-RECENCYX
           ELSE
           IF RDPOTCHGOFF = "M"
              MOVE "<PL>" TO D-RECENCYX
           ELSE
              MOVE RECDEL TO D-RECENCY.
           MOVE LN-RECPROFILE(1) TO D-PYRECPRO.
           INSPECT D-XPYRECPRO REPLACING ALL "9" BY "-".

      * DETERMINE ACCOUNT STATUS (IB-PC):
           MOVE TRANS-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.

           MOVE SAVE-UNEARN(1) TO UNEARN-INT.
           MOVE SAVE-UNEARN(3) TO UNEARN-SC.
           MOVE SAVE-UNEARN(4) TO UNEARN-MF.

           ADD UNEARN-INT UNEARN-SC UNEARN-MF GIVING D-UNEARNED.

           MOVE LN-CONPROFILE(1) TO D-PROFILE.
           INSPECT D-XPROFILE REPLACING ALL "9" BY "-".

           IF CPOT30 = "Y"
              MOVE "<P>" TO D-CONTUALX
           ELSE
           IF CPOT30 = "M"
              MOVE "<M>" TO D-CONTUALX
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-CONTUALX
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "<PL>" TO D-CONTUALX
           ELSE
              MOVE CONTRACTUAL TO D-CONTUAL.

      * EXAMPLE HOLD-LP-PDTH-DATE = 20170522
      * IF UNITPER = DAYS THEN YOU WANT 00000522 - JUST 0 OUT CCYY
      * IF NOT DAYS THEN YOU WANT 00000517 - MOVE YEAR TO DAY
           MOVE HOLD-LP-PDTH-DATE TO DATE-YYYYMMDD.
           IF NOT LN-UNITPER-CD-DAYS
              MOVE DATE-YYYYMMDD-YY TO DATE-YYYYMMDD-DD.
           MOVE 0 TO DATE-YYYYMMDD-CCYY.
           MOVE DATE-YYYYMMDD TO D-PDTHRU.
           IF LN-OTHBAL NOT = 0
              MOVE LN-OTHBAL TO D-OTHBAL.
           IF LN-INTBAL NOT = 0 OR LN-LCBAL NOT = 0
              ADD LN-INTBAL LN-LCBAL GIVING D-INTBAL.
           IF LN-CURBAL NOT = 0
              MOVE LN-CURBAL TO D-CURBAL.
       DETAIL-LN-ACCTOT.
           MOVE 1 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 2 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 3 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           PERFORM WRITE-DETAIL-LINE.
           IF PD-SSNO(2) NOT = 0
              MOVE 2 TO BW-SUB
              PERFORM PROCESS-BORROWER
              PERFORM WRITE-DETAIL-LINE.
           IF PD-SSNO(3) NOT = 0
              MOVE 3 TO BW-SUB
              PERFORM PROCESS-BORROWER
              PERFORM WRITE-DETAIL-LINE.
           IF PD-SSNO(4) NOT = 0
              MOVE 4 TO BW-SUB
              PERFORM PROCESS-BORROWER
              PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ***************************************
       DETAIL-LN-ACCTBL SECTION.
      ***************************************
           ADD 1 TO T-NO(LEV SAVE-CSUB 1).
           ADD LN-OTHBAL TO T-OTH(LEV SAVE-CSUB 1).
           ADD LN-INTBAL LN-LCBAL TO T-INT(LEV SAVE-CSUB 1).
           ADD LN-CURBAL TO T-BAL(LEV SAVE-CSUB 1).
           ADD UNEARN-INT TO T-UNE(LEV SAVE-CSUB 1).
           ADD UNEARN-SC TO T-IER(LEV SAVE-CSUB 1).
           ADD UNEARN-MF TO T-ERN(LEV SAVE-CSUB 1).

           ADD 1 TO T-NO(LEV SAVE-RDSUB 2).
           ADD LN-OTHBAL TO T-OTH(LEV SAVE-RDSUB 2).
           ADD LN-INTBAL LN-LCBAL TO T-INT(LEV SAVE-RDSUB 2).
           ADD LN-CURBAL TO T-BAL(LEV SAVE-RDSUB 2).
           ADD UNEARN-INT TO T-UNE(LEV SAVE-RDSUB 2).
           ADD UNEARN-SC TO T-IER(LEV SAVE-RDSUB 2).
           ADD UNEARN-MF TO T-ERN(LEV SAVE-RDSUB 2).
           IF SAVE-PSUB = 0 OR SP-BANKRULE = "D"
              GO TO DETAIL-LN-ACCTBL-EXIT.

           ADD 1 TO T-NO(LEV SAVE-PSUB 3).
           ADD LN-OTHBAL TO T-OTH(LEV SAVE-PSUB 3).
           ADD LN-INTBAL LN-LCBAL TO T-INT(LEV SAVE-PSUB 3).
           ADD LN-CURBAL TO T-BAL(LEV SAVE-PSUB 3).
           ADD UNEARN-INT  TO T-UNE(LEV SAVE-PSUB 3).
           ADD UNEARN-SC  TO T-IER(LEV SAVE-PSUB 3).
           ADD UNEARN-MF TO T-ERN(LEV SAVE-PSUB 3).

       DETAIL-LN-ACCTBL-EXIT.
           EXIT.

      ***************************************
       EXCEPTION-ROUTINE SECTION.
      ***************************************
           MOVE WK-ACCTNO TO D-NUMBER.
           PERFORM WRITE-DETAIL-LINE.
           PERFORM DELETE-WK-FILE.
       EXCEPTION-ROUTINE-EXIT.
           EXIT.

      ****************************
       PROCESS-BORROWER SECTION.
      ****************************
           PERFORM OPEN-BW1-FILE.

           MOVE PD-SSNO(BW-SUB) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF NOT IO-GOOD
              MOVE "**NOT ON FILE**" TO D-NAME
              GO TO END-PROCESS-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
               BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME.

       END-PROCESS-BORROWER.
           EXIT.

      *******************************
      *    VERIFY BRANCH IS OPEN
      *******************************
       VERIFY-BRANCH-IS-OPEN SECTION.
           MOVE " " TO BR-OPEN-FG.
           MOVE "A1"       TO RC-STATUS.
           MOVE EXT-CONAME-USER-LOC TO RC-BRNO.
           MOVE TRANS-DATE          TO RC-TRANS-DATE.
           PERFORM OPEN-RC2-FILE.
           PERFORM READ-RC2-FILE.
           PERFORM CLOSE-RC2-FILE.
           IF IO-FG NOT = 0
              MOVE "N" TO BR-OPEN-FG.

       VERIFY-BRANCH-IS-OPEN-EXIT.
           EXIT.

      *******************************
       HEAD-PAGE SECTION.
      *******************************

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
           IF SUMMARY-FG = "N"
              MOVE 2 TO LN-FEED
              MOVE HEAD5 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 1 TO LN-FEED
              MOVE HEAD6 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 7 TO LN-COUNT
              MOVE 2 TO LN-FEED
            ELSE
              MOVE 5 TO LN-COUNT
              MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 4 TO LN-FEED.

           MOVE "FORM REQUEST NO.   :" TO RANGE-LINE.
           MOVE FORMNO                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.


      ***************************************
       SET-BRANCH-HEADINGS SECTION.
      ***************************************

           MOVE HOLD-BRNO TO BR-NO H-BRNO.
           PERFORM READ-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.
           MOVE BR-NAME TO H-BRDESC.


      ****************************************
       COMPANY-TOTALS SECTION.
      ****************************************

           MOVE "Y" TO SUMMARY-FG.
           MOVE SPACES TO H-BRNOX.
           MOVE "**** ALL BRANCHES ****" TO H-BRDESC.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE "COMPANY TOTALS:" TO S-SUMNAME.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 3 TO LEV.
           PERFORM PRINT-AGEING-SUMMARY.

      ****************************************
       PRINT-AGEING-SUMMARY SECTION.
      ****************************************

           PERFORM ZERO-TT-TOTALS.
           PERFORM ACCUM-AGEING-TOTALS
              VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
                AFTER TYP FROM 1 BY 1 UNTIL TYP > 3.
           MOVE 1 TO TYP.
           MOVE "********* CONTRACTUAL DELINQUENCY *********"
                                                       TO S-AGETYPE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-DETAIL-SUM VARYING AGE
              FROM 1 BY 1 UNTIL AGE > 10.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
           MOVE "  ********* RECENCY DELINQUENCY *********"
                                                      TO S-AGETYPE.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-DETAIL-SUM
              VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
                AFTER TYP FROM 2 BY 1 UNTIL TYP > 3.
           MOVE 2 TO TYP.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
           MOVE 3 TO TYP.
           MOVE 1 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.

      ****************************************
       PRINT-DETAIL-SUM SECTION.
      ****************************************

           IF T-NO(LEV AGE TYP) = 0
              IF T-BAL(LEV AGE TYP) = 0
                 IF T-INT(LEV AGE TYP) = 0
                    IF T-OTH(LEV AGE TYP) = 0
                       IF T-UNE(LEV AGE TYP) = 0
                          IF T-ERN(LEV AGE TYP) = 0
                             IF T-IER(LEV AGE TYP) = 0
                                GO TO END-PRINT-DETAIL-SUM.
           IF TYP = 3
              MOVE "  PAID-AHEAD" TO S-NAME
            ELSE
              MOVE AGE-DESC(AGE) TO S-NAME.
           IF T-NO(LEV AGE TYP) NOT = 0
              MOVE T-NO(LEV AGE TYP) TO S-NO
              IF TYP NOT = 3
                 IF TT-NOS(TYP) NOT = 0
                    COMPUTE S-NO-P ROUNDED =
                        ( T-NO(LEV AGE TYP) / TT-NOS(TYP) ) * 100
                   ELSE
                    MOVE 0 TO S-NO-P.
           IF T-BAL(LEV AGE TYP) NOT = 0
              MOVE T-BAL(LEV AGE TYP) TO S-BAL
              IF TYP NOT = 3
                 IF TT-BAL(TYP) NOT = 0
                    COMPUTE S-BAL-P ROUNDED =
                        ( T-BAL(LEV AGE TYP) / TT-BAL(TYP) ) * 100
                   ELSE
                    MOVE 0 TO S-BAL-P.
           IF T-INT(LEV AGE TYP) NOT = 0
              MOVE T-INT(LEV AGE TYP) TO S-INT
              IF TYP NOT = 3
                 IF TT-INT(TYP) NOT = 0
                    COMPUTE S-INT-P ROUNDED =
                        ( T-INT(LEV AGE TYP) / TT-INT(TYP) ) * 100
                   ELSE
                    MOVE 0 TO S-INT-P.
           IF T-OTH(LEV AGE TYP) NOT = 0
              MOVE T-OTH(LEV AGE TYP) TO S-OTH
              IF TYP NOT = 3
                 IF TT-OTH(TYP) NOT = 0
                    COMPUTE S-OTH-P ROUNDED =
                        ( T-OTH(LEV AGE TYP) / TT-OTH(TYP) ) * 100
                   ELSE
                    MOVE 0 TO S-OTH-P.
           IF T-UNE(LEV AGE TYP) NOT = 0
              MOVE T-UNE(LEV AGE TYP) TO S-UNE
              IF TYP NOT = 3
                 IF TT-UNE(TYP) NOT = 0
                    COMPUTE S-UNE-P ROUNDED =
                        ( T-UNE(LEV AGE TYP) / TT-UNE(TYP) ) * 100
                   ELSE
                    MOVE 0 TO S-UNE-P.
           IF T-ERN(LEV AGE TYP) NOT = 0
              MOVE T-ERN(LEV AGE TYP) TO S-ERN
              IF TYP NOT = 3
                 IF TT-ERN(TYP) NOT = 0
                    COMPUTE S-ERN-P ROUNDED =
                        ( T-ERN(LEV AGE TYP) / TT-ERN(TYP) ) * 100
                   ELSE
                    MOVE 0 TO S-ERN-P.
           IF T-IER(LEV AGE TYP) NOT = 0
              MOVE T-IER(LEV AGE TYP) TO S-IER
              IF TYP NOT = 3
                 IF TT-IER(TYP) NOT = 0
                    COMPUTE S-IER-P ROUNDED =
                        ( T-IER(LEV AGE TYP) / TT-IER(TYP) ) * 100
                   ELSE
                    MOVE 0 TO S-IER-P.
              PERFORM WRITE-DETAIL-LINE.
       END-PRINT-DETAIL-SUM.
           EXIT.


      **************************************
       CREATE-FM-REC SECTION.
      **************************************

           IF LN-CONFIDENTCD = "Y" OR BW-CONFIDENTIAL = "Y"
              GO TO END-CREATE-FM-REC.

           PERFORM OPEN-FM1-FILE.

           PERFORM CLEAR-FM-FILE.

           MOVE SPACES TO LPWSFM-EDIT-FM-INITKEY.

           IF FD-1STFILE = 2
              MOVE LN-SSNO(1)  TO LPWSFM-EDIT-FM-SSNO
           ELSE
              MOVE LN-ACCTNO   TO LPWSFM-EDIT-FM-ACCTNO
              MOVE FORMNO      TO LPWSFM-EDIT-FM-FORMNO.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE FORMNO                 TO FM-FMNO.

           MOVE FM1-KEY TO HOLD-FM1-KEY.

           PERFORM READ-FM1-FILE.

      * PRINT FORMS IN ZIP CODE ORDER
           MOVE BW-ZIP             TO FM-ZIP.
           MOVE SPACES             TO FM-PRINT.
           MOVE 0                  TO FM-STMT-COUNTER.
           MOVE TRANS-DATE         TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.
           IF IO-GOOD
              PERFORM REWRITE-FM1-FILE
           ELSE
              MOVE HOLD-FM1-KEY TO FM1-KEY
              PERFORM WRITE-FM1-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.
           PERFORM CLOSE-FM1-FILE.

       END-CREATE-FM-REC.
           EXIT.


      *******************************
       VERIFY-CLASS SECTION.
      *******************************
           MOVE "00" TO STAT.
           MOVE TEST-LN-CLASS TO LC-CODE.
           PERFORM GET-LC.
           IF IO-BAD
              MOVE "N" TO STAT
              GO TO VERIFY-CLASS-EXIT.

       VERIFY-CLASS-EXIT.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBGB/SCREEN.CPY".

       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBLP/LPSRCD.CPY".
       COPY "LIBLP/LPTRJCL.CPY".
       COPY "LIBLP/LPTYCL.CPY".
       COPY "LIBLP/LPDSCL.CPY".
       COPY "LIBLP/LPFMCL.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO CLAST2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLAST2-SYSDATE.
           DISPLAY CLAST2-SCREEN UPON CLAST2-WINDOW-HANDLE.
           MOVE CLAST2-FORM-FIELDS TO WR-BUF. 
           MOVE CLAST2-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE CLAST2-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLAST2-HELP--DIR  TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CLAST2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************
       MISC-ROUTINES SECTION.
      *****************************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/RMFILE.CPY".
       ZERO-TT-TOTALS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              MOVE 0 TO TT-NOS(SUB) TT-BAL(SUB) TT-INT(SUB)
                        TT-OTH(SUB) TT-UNE(SUB) TT-ERN(SUB)
                        TT-IER(SUB)
           END-PERFORM.
       ZERO-DD-TOTALS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 32
              MOVE 0 TO DD-NO(SUB) DD-BAL(SUB) DD-INT(SUB)
                        DD-OTH(SUB) DD-UNE(SUB)
           END-PERFORM.
       ZERO-NM-TOTALS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 27
              MOVE 0 TO NM-NO(SUB) NM-BAL(SUB) NM-INT(SUB)
                        NM-OTH(SUB) NM-UNE(SUB)
           END-PERFORM.
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
      *       UNLOCK SPA-FILE
      *       UNLOCK SPB-FILE
      *       UNLOCK SPC-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.

       GET-RECOURSE-SUB.
           IF PD-DLRECOURSE = "F"
              MOVE 1 TO SUB
           ELSE
              IF PD-DLRECOURSE = "P"
                 MOVE 2 TO SUB
              ELSE
                 IF PD-DLRECOURSE = "N"
                    MOVE 3 TO SUB
                 ELSE
                    MOVE 0 TO SUB.

       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
       SETUP-DS-DATE-CCYYMM.
           MOVE TRANS-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM.
       SET-FRTY-KEY.
           MOVE TRANS-DATE TO TY-DATE.
           MOVE PD-CLASS   TO TY-CLASS.
       SET-TOTY-KEY.
           MOVE TRANS-DATE TO TY-DATE.
           MOVE TO-LC      TO TY-CLASS.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       PRINT-DUE-TOTALS.
           MOVE "      TOTAL:" TO S-NAME.
           MOVE TT-NOS(1) TO S-NO.
           MOVE TT-BAL(1) TO S-BAL.
           MOVE TT-INT(1) TO S-INT.
           MOVE TT-OTH(1) TO S-OTH.
           MOVE TT-UNE(1) TO S-UNE.
           PERFORM WRITE-DETAIL-LINE.
       PRINT-SUM-TOTALS.
           IF TYP = 3
              MOVE "  PAID-AHEAD" TO S-NAME
           ELSE
              MOVE "      TOTAL:" TO S-NAME.
           MOVE TT-NOS(TYP) TO S-NO.
           MOVE TT-BAL(TYP) TO S-BAL.
           MOVE TT-INT(TYP) TO S-INT.
           MOVE TT-OTH(TYP) TO S-OTH.
           MOVE TT-UNE(TYP) TO S-UNE.
           MOVE TT-ERN(TYP) TO S-ERN.
           MOVE TT-IER(TYP) TO S-IER.
           PERFORM WRITE-DETAIL-LINE.
       ACCUM-AGEING-TOTALS.
           ADD T-NO(LEV AGE TYP) TO TT-NOS(TYP).
           ADD T-BAL(LEV AGE TYP) TO TT-BAL(TYP).
           ADD T-INT(LEV AGE TYP) TO TT-INT(TYP).
           ADD T-OTH(LEV AGE TYP) TO TT-OTH(TYP).
           ADD T-UNE(LEV AGE TYP) TO TT-UNE(TYP).
           ADD T-ERN(LEV AGE TYP) TO TT-ERN(TYP).
           ADD T-IER(LEV AGE TYP) TO TT-IER(TYP).
       RESET-TOT-PAGE.
           MOVE ZEROS TO TOTAL-TBL(3).
           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.


      *****************************************************
       IO-ROUTINE SECTION.
      *****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRJGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBLP/LPEDPATHIO.CPY".
       COPY "LIBLP/LPIOCLAS.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPTY1IN.CPY".
       COPY "LIBLP/LPDS1IN.CPY".
       COPY "LIBLP/LPWKIN.CPY".
       COPY "LIBLP/LPTRJ1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *****************************************************
       END-PROGRAM SECTION.
      *****************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CLAST2-SCREEN.
           DESTROY CLAST2-WINDOW-HANDLE.
           EXIT PROGRAM.
