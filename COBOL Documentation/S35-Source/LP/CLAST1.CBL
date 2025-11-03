       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CLAST1.
       DATE-WRITTEN. 10-13-94
      ************************************************************
      *    DIR :  LP
      *    DESC:  CLASS TRANSFER
      *           SELECT LOANS TO BE TRANSFERRED TO ANOTHER CLASS
      *
      * REV:
      *  101494 BAH REMOVED ALL OCCURANCES OF LP2
      *  061695 JTG ADDED SET OF CRNORC-BR-NOT-OURS-REFUND
      *  072895 JTG ADDED LOGIC FOR DELSUB-SET ROUTINE
      *  081395 JTG CORRECTED TO SET WK-TO-DLNO FOR CLAST2.C
      *  083195 KEC ADDED LIBLP/LPNOCL
      *  049996 MJD ADDED PASSWORD TO ACCOUNT RANGES.
      *             RANGE OF MORE THAN 1 ACCOUNT NEEDS PASSWORD.
      *  MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      *  JTG 091196 ADDED P&L ONLY OPTION
      *             ADDED COPY OF "LIBLP/CLASTRW"
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER READING BWFILE.
      *  JTG 030797 ALLOWED TRANSFER OF REPO ACCOUNTS
      *  MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      *  GLF 090397 REMOVED ALL USE OF GLOBAL
      *  MJD 990730 ADDED GPENV(W). NEEDED FOR CHANGES TO LPEARN.
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BAH 991230 ADDED OPTION TO I,S,O ZERO BALANCE ACCOUNTS, TFC PR# 62
      *             DONT ALLOW A CLASS TRANSFER ON RESCISSION ACCOUNTS
      *  BAH 000110 AGEING TOTALS WERE OLD SUBSCRIPTS FROM PRIOR TO
      *             WORLD, CAUSING INDEX OUT OF BOUNDS LOCK UPS !!
      *             TENFIN (TN) JUDY
      *  BLV 000306 VALIDATE GI FOR TARGET CLASS, IF INVALID, SKIP LOAN &
      *             DOCUMENT ON REPORT
      *  BAH 040819 DO NOT CREATE FORM REQUEST IF LOAN OR BORROWER IS
      *             CONFIDENTIAL, PR# 2343
      *  BAH 041118 IF CHG-SPR-LC = "Y" AND BUILD-SPR = "N", VERIFY THAT
      *             THE TARGET CLASS SPR EXISTS OR WRITE EXCEPTION AND
      *             SKIP IT...AUTO CREDIT PR# 2421
      *   CS 101107 ADDED 'FROM' CLASS RANGE SO THE CLASS TRANSFER DID NOT
      *             NEED RUN FOR EACH CLASS BEING TRANSFERRED TO THE IMPAIRED
      *             ASSET CLASS. ALL TESTS ON FROM BRANCH THAT WERE CHECK IN
      *             CLASTR ARE NOW DONE HERE   REGIONAL ACCPT PR#277
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0216 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *         ****NOTE**** FORMNO NOT CHANGED (SCREEN EDIT).
      *  CS 160823 CHANGED FR-LC TO 999,FR-LC-X XXX, FR2-LC PIC 999,
      *            TO-LC PIC 999
      *  BAH 170517 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181114 GLOBAL FMFILE
      *  BAH 181205 GLOBAL LNFILE
      *  BAH 190312 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      *  BAH 190613 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      *  BAH 201119 OPEN-WK-FILE-OUTPUT DUE TO NO CREATE ON I-O IN A34
      *  BAH 2024.0201 REMOVED CRNOR2 FOR A FORM
      *  BAH 2025.0313 REMOVED NOTICE FILE, NOT USED S35Q-176
      ************************************************************
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
           "@(#)LP/CLAST1.CBL S34 10/11/21-15:57:37 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBLP/LPWSCLAS.CPY".
       01  WK-IFN             PIC X(2) VALUE "WK".
       01  PR-IFN             PIC X(2) VALUE "PR".

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
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  MESSX-LIST           PIC X(6)     VALUE "MESSX.".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).
       01  TRANS-DATE           PIC 9(8)     COMP-3 VALUE 0.
       01  VALID-REC-FG         PIC X.
       01  TO-LC-SMALLOAN       PIC X.
       01  TO-LC-SALEFIN        PIC X.
       01  TO-LC-SERVACCT       PIC X.
       01  TO-LC-LOANTYPE       PIC X.

       01  NOSEL                PIC S9(9).
       01  AMTSEL               PIC S9(9)V99.

       01  HOLD-BRNO            PIC 9(4)     VALUE 0.

       01  HOLD-FM1-KEY         PIC X(28)     VALUE SPACES.

       01  HOLD-LPS-NUM.
           03  H-LPS-CLASS      PIC XXX.
           03  H-LPS-NUM        PIC X(13).

       01  SAVE-LN-ACCTNO       PIC 9(8).

       01  SAVE-RECDEL          PIC 9(4)     VALUE 0.
       01  SAVE-RDSUB           PIC 99       VALUE 0.
       01  SAVE-CSUB            PIC 99       VALUE 0.
       01  SAVE-PSUB            PIC 99       VALUE 0.
       01  SAVE-OUTBAL          PIC S9(7)V99 VALUE 0.
       01  SAVE-PASTDUE         PIC S9(7)V99 VALUE 0.

       01  UNEARN-INT           PIC S9(7)V99 COMP-3 VALUE 0.
       01  UNEARN-SC            PIC S9(7)V99 COMP-3 VALUE 0.
       01  UNEARN-MF            PIC S9(7)V99 COMP-3 VALUE 0.

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

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  ERRCD                  PIC X.

       01  SCREEN-FIELDS.
           03  POOLID            PIC 9(5).
           03  DEALNO            PIC 9999V99.
           03  CHG-SPR-LC        PIC X.
           03  BUILD-SPR         PIC X.
           03  BEG-ZIPCODE       PIC X(10).
           03  END-ZIPCODE       PIC X(10).
           03  PL-ONLY           PIC X.
           03  BEG-SUBCLASS      PIC 9(2).
           03  END-SUBCLASS      PIC 9(2).
           03  BEG-ACCTNO        PIC 9(6).
           03  END-ACCTNO        PIC 9(6).
           03  BEG-CONTRACT      PIC 9(4).
           03  END-CONTRACT      PIC 9(4).
           03  BEG-RECENCY       PIC 9(4).
           03  END-RECENCY       PIC 9(4).
           03  BEG-DLNO          PIC 9999V99.
           03  END-DLNO          PIC 9999V99.
           03  FORMNO            PIC 9(3).
           03  ONHAND            PIC X.
           03  SOLD              PIC X.
           03  REDEEM            PIC X.
           03  ZBAL              PIC X.

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
           03  FILLER           PIC X(31)    VALUE
           "CLASS TRANSFER SELECTION FROM ".
           03  H-FRLC           PIC Z9.
           03  H-DASH           PIC X       VALUE "-".
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

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM  PIC X(13).
           03  FILLER      PIC X.
           03  D-DUEDAY    PIC 99.
           03  FILLER      PIC XX.
           03  D-NAME      PIC X(19).
           03  FILLER      PIC X.
           03  D-DLNO      PIC ZZZ9.99.
           03  FILLER      PIC X.
           03  D-PAYDATE   PIC 99/99/99.
           03  FILLER      REDEFINES D-PAYDATE.
               05  FILLER  PIC XX.
               05  D-DASH1 PIC X.
               05  FILLER  PIC XX.
               05  D-DASH2 PIC X.
               05  FILLER  PIC XX.
           03  D-RECENCYX.
               05  D-RECENCY   PIC ZZZZ.
           03  FILLER      PIC X.
           03  D-PYRECPRO  PIC 9(12).
           03  D-XPYRECPRO REDEFINES D-PYRECPRO PIC X(12).
           03  D-UNEARNED  PIC ZZZZ9.99-.
           03  D-PROFILE   PIC 9(12).
           03  D-XPROFILE  REDEFINES D-PROFILE PIC X(12).
           03  D-CONTUALX.
               05  D-CONTUAL   PIC ZZZZ.
           03  D-PDTHRU    PIC ZZ9999.
           03  D-OTHBAL    PIC ZZZZZ9.99-.
           03  D-INTBAL    PIC ZZZZZ9.99-.
           03  D-CURBAL    PIC ZZZZZZ9.99.

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

       01  ERROR-LINE REDEFINES DETAIL-LINE.
           03  D-ERROR-ACCT     PIC X(16).
           03  D-GI-ERROR1      PIC X(55).
           03  D-GI-ERROR2      PIC X(61).

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(112).
           03  RANGE-DEALER REDEFINES RANGE-SELECTION PIC Z(4).99.

       01  SUMMARY1.
           03  FILLER           PIC X(15)  VALUE SPACES.
           03  FILLER           PIC X(48)  VALUE
           "NO----%     OTH DUE----%  INT/LC DUE----%".
           03  FILLER           PIC X(17)  VALUE
           "BAL DUE----%   UN".
           03  FILLER           PIC X(51)  VALUE
           "EARN-INT----%    UNEARN-SC----%    UNEARN-MF----%".

       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBWI/FDSCANW.CPY".

       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01   BR-SEQ            PIC 9  VALUE 0.
      * SEQUENCE 1 IS IN LP/CLASTR.C FOR TRANSFERRING ACROSS MACHINES.
       01   ACCT-SEQ          PIC 9  VALUE 2.
      * WHEN ADDING PASSWORD SEQUENCES, THIS CLAST1 PROGRAM USES "CLASTR"
      * AS THE PROGRAM IN THE PASSWORD FILE, SO YOU MUST MAKE SURE
      * THE PROGRAM LP/CLASTR.C DOES NOT ALREADY USE THE SEQUENCE YOUR
      * ADDING.

       01  CLAST1-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CLAST1_DEF.CPY".
       COPY "LIBLP/CLAST1_WKS.CPY".
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
       COPY "LIBLP/CLAST1_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME TRANSFER-BUF EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CLAST1" TO FORM-PROG VDU-FORM-NAME.

      * MADE FLOATING SO IT CAN RETURN TO CLASTR STILL FILLED OUT
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CLAST1-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO END-PROGRAM.

           PERFORM CLAS-TRANSFER.

      * CREATE HEADER RECORD WORK FILE
           MOVE " "        TO WK-TYPE.
           MOVE NOSEL      TO WK-NOSEL.
           MOVE AMTSEL     TO WK-AMTSEL.
           MOVE TO-DLNO    TO WK-TO-DLNO.
           MOVE PL-ONLY    TO WK-PL-ONLY.
           MOVE ONHAND     TO WK-ONHAND.
           MOVE SOLD       TO WK-SOLD.
           MOVE REDEEM     TO WK-REDEEM.
           MOVE CHG-SPR-LC TO WK-CHG-SPR-LC.
           MOVE BUILD-SPR  TO WK-BUILD-SPR.
           MOVE ZBAL       TO WK-ZBAL.
           PERFORM WRITE-WK-FILE.

           IF FIRST-TIME = "N"
              PERFORM COMPANY-TOTALS.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.


      ******************************************
       CLAS-TRANSFER SECTION.
      ******************************************


           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.

           MOVE LN-PATH-OWNBR   TO LN-OWNBR
                                   QLN3-WBEG-OWNBR
                                   QLN3-WEND-OWNBR.
           MOVE FR-LC           TO LN-CLASS
                                   QLN3-WBEG-CLASS.
           MOVE FR2-LC          TO QLN3-WEND-CLASS.
           MOVE BEG-ACCTNO      TO SAVE-LN-ACCTNO
                                   LN-ACCTNO
                                   QLN3-WBEG-ACCTNO.
           MOVE END-ACCTNO      TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       CLAS-TRANSFER-NEXT.
           PERFORM READ-LN3-FILE-NEXT.
           IF (IO-BAD) OR
               (LN-ACCTNO > END-ACCTNO) OR
                (LN-CLASS > FR2-LC) OR
                 (LN-OWNBR NOT = LN-PATH-OWNBR)
              PERFORM CLOSE-LN3-FILE
              GO TO CLAS-TRANSFER-EXIT.

           IF LN-CLASS = TO-LC
              GO TO CLAS-TRANSFER-NEXT.

            IF LN-CLASS NOT = LPWSSP-SPRCLASS
               MOVE LN-CLASS TO LC-CODE
               PERFORM READ-LC1-FILE
               IF IO-BAD
                  MOVE "INVALID LOAN CLASS, CLASS SKIPPED"
                                                      TO D-ERROR-ACCT
                  GO TO CLAS-TRANSFER-NEXT
               ELSE
                  IF NOT (LC-SALEFIN = TO-LC-SALEFIN AND
                           LC-SERVACCT = TO-LC-SERVACCT AND
                            LC-LOANTYPE = TO-LC-LOANTYPE AND
                             LC-SMALLOAN = TO-LC-SMALLOAN)
                  GO TO CLAS-TRANSFER-NEXT.

             MOVE LN-CLASS TO LPWSSP-SPRCLASS.


      * DO NOT TRANSFER VOIDS
      * DO NOT TRANSFER ACCOUNTS IN RESCISSION
      * DO NOT TRANSFER ACCOUN "PB"'D INTO A RESCISSION
      * DO NOT TRANSFER ACCOUNTS ALREADY TRANSFERRED INTO ANOTHER BRANCH
           IF LN-POCD-NONLOAN OR LN-POCD-PB-RESC OR LN-POCD-TRANSFERRED
              GO TO CLAS-TRANSFER-NEXT.

           IF LN-SUBCLASS < BEG-SUBCLASS
                  OR LN-SUBCLASS > END-SUBCLASS
              GO TO CLAS-TRANSFER-NEXT.

           IF LN-DLNO < BEG-DLNO
                  OR LN-DLNO > END-DLNO
              GO TO CLAS-TRANSFER-NEXT.

      *---------------------------------------------------------
      * TEST FOR OPTION OF ZERO BALANCE ONLY:
           IF ZBAL = "O"
              IF LN-CURBAL NOT = 0
                 GO TO CLAS-TRANSFER-NEXT
              END-IF
           ELSE
      * TEST FOR OPTION OF NO ZERO BALANCE ACCOUNTS:
           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 GO TO CLAS-TRANSFER-NEXT.

      *---------------------------------------------------------
      * TEST FOR OPTION OF P&L ONLY:
           IF PL-ONLY = "O"
              IF LN-PLCD = " " OR LN-PLDATE = 0
                 GO TO CLAS-TRANSFER-NEXT
              END-IF
           ELSE
      * TEST FOR OPTION OF NO P&L ACCOUNTS:
           IF PL-ONLY = "S"
              IF (LN-PLCD NOT = " ") OR (LN-PLDATE NOT = 0)
                 GO TO CLAS-TRANSFER-NEXT.

      *---------------------------------------------------------
      * JTG 030797 ALLOWEWD TRANSFER OF REPO ACCOUNTS
      * SKIP REPO (ONHAND OR SOLD) ACCOUNTS THAT ARE NOT IN P&L:
      *
      *    IF LN-REPOCD = "X" OR "S"
      *       IF LN-PLCD = " " OR LN-PLDATE = 0
      *       GO TO CLAS-TRANSFER-NEXT.

           IF ONHAND = "O"
              IF LN-REPOCD NOT = "X"
                 GO TO CLAS-TRANSFER-NEXT.

           IF SOLD = "O"
              IF LN-REPOCD NOT = "S"
                 GO TO CLAS-TRANSFER-NEXT.

           IF REDEEM = "O"
              IF LN-REPOCD NOT = "V"
                 GO TO CLAS-TRANSFER-NEXT.

           IF ONHAND = "S"
              IF LN-REPOCD = "X"
                 GO TO CLAS-TRANSFER-NEXT.

           IF SOLD = "S"
              IF LN-REPOCD = "S"
                 GO TO CLAS-TRANSFER-NEXT.

           IF REDEEM = "S"
              IF LN-REPOCD = "V"
                 GO TO CLAS-TRANSFER-NEXT.

      *---------------------------------------------------------

           IF POOLID NOT = 99999
              IF LN-DLPOOL NOT = POOLID
                 GO TO CLAS-TRANSFER-NEXT.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              GO TO CLAS-TRANSFER-NEXT.

           IF BW-ZIP < BEG-ZIPCODE OR BW-ZIP > END-ZIPCODE
              GO TO CLAS-TRANSFER-NEXT.

           IF CHG-SPR-LC = "Y" AND BUILD-SPR = "N"
              PERFORM VERIFY-SPR
              IF VALID-REC-FG = "N"
                 PERFORM LPSTAT-DISPLAY
                 MOVE LPS-DISPLAY-LNNO TO D-ERROR-ACCT
                 MOVE
                 "INVALID SPR FOR TARGET CLASS;" TO D-GI-ERROR1
                 MOVE "ACCOUNT SKIPPED!!" TO D-GI-ERROR2
                 PERFORM WRITE-DETAIL-LINE
                 GO TO CLAS-TRANSFER-NEXT.

           PERFORM VERIFY-GI.
           IF VALID-REC-FG = "N"
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D-ERROR-ACCT
              MOVE
              "INVALID GL INTERFACE FOR TARGET CLASS;"
                TO D-GI-ERROR1
              MOVE "ACCOUNT SKIPPED!!" TO D-GI-ERROR2
              PERFORM WRITE-DETAIL-LINE
              GO TO CLAS-TRANSFER-NEXT.

      * ESTABLISH CONTR, RECENCY & PAST DUE FOR UPDATE-TT1:
           PERFORM GET-SP.
           PERFORM ESTABLISH-ORIG-TT-NUMBERS.

           IF SAVE-RECDEL < BEG-RECENCY OR SAVE-RECDEL > END-RECENCY
              GO TO CLAS-TRANSFER-NEXT.

           IF CONTRACTUAL < BEG-CONTRACT OR CONTRACTUAL > END-CONTRACT
              GO TO CLAS-TRANSFER-NEXT.


           MOVE LN-ACCTNO TO SAVE-LN-ACCTNO.

           IF NOT (PRU-LP = "VDU" OR "PRU")
              STRING "PROGRAM IN PROGRESS..." LN-OWNBR " " LN-ACCTNO
                  "                                "
                   DELIMITED BY SIZE INTO MESS
              PERFORM SEND-LEGENDX.

           IF FIRST-TIME = "Y"
              MOVE "Y" TO RECORDS-FOUND
              MOVE LN-OWNBR TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS.

           PERFORM DETAIL-LINE-PROCESS.

      * TEST FOR FORM REQUEST:
           IF FORMNO NOT = 0
              PERFORM CREATE-FM-REC.

      * PUT ACCOUNT INTO WORK FILE:
           MOVE "D" TO WK-TYPE.
           MOVE SAVE-LN-ACCTNO TO WK-ACCTNO.
           MOVE 0 TO WK-AMTSEL.
           PERFORM WRITE-WK-FILE.

           ADD 1 TO NOSEL.
           ADD LN-CURBAL TO AMTSEL.

           GO TO CLAS-TRANSFER-NEXT.

       CLAS-TRANSFER-EXIT.
           EXIT.

      ******************************************************
      *    ESTABLISH CONTR, RECENCY & PAST DUE
      ******************************************************
       ESTABLISH-ORIG-TT-NUMBERS SECTION.
           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.
           MOVE RECDEL TO SAVE-RECDEL.

           PERFORM COMPUTE-PAID-AHEAD.
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE TRANS-DATE   TO CEPP2-DATE.
           PERFORM CONTRACTUAL-DELINQUENCY.
           IF CDEL-DELINQUENT < 0
              SUBTRACT CDEL-DELINQUENT FROM 0 GIVING SAVE-PASTDUE
           ELSE
              MOVE 0 TO SAVE-PASTDUE.

           MOVE CSUB TO SAVE-CSUB.
           MOVE RDSUB TO SAVE-RDSUB.
           MOVE PSUB TO SAVE-PSUB.

           MOVE LN-CURBAL TO SAVE-OUTBAL.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE "N" TO RECORDS-FOUND.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO AGEING-DATE
                                 TRANS-DATE.

           MOVE "SNNN020FRLC." TO VDU.
           MOVE FR-LC TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020FRLC2." TO VDU.
           MOVE FR2-LC  TO VDUNUM0.
           PERFORM SCREENIO.

      * SET 'TO' CLASS:
           MOVE "Y" TO VALID-REC-FG.
           MOVE TO-LC TO LC-CODE.
      * MUST KEEP THIS OPEN HERE, OTHER OPEN IS FOR MAIN LOOP
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           IF IO-BAD
              MOVE "PROBLEM GETTING 'TO' CLASS" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.
           PERFORM CLOSE-LC1-FILE.

      *-- EVERYTIME WE CHANGE CLASSES COMPARE TO THESE VALUES TO
      *   DETERMINE IF 'LIKE' CLASS.
           MOVE LC-SMALLOAN TO TO-LC-SMALLOAN.
           MOVE LC-SALEFIN  TO TO-LC-SALEFIN.
           MOVE LC-SERVACCT TO TO-LC-SERVACCT.
           MOVE LC-LOANTYPE TO TO-LC-LOANTYPE.
      *--

           MOVE "SNNN020TOLC." TO VDU.
           MOVE TO-LC TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A180TOLCN." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.

       SET-WK-FILE.
           MOVE FR-LC     TO CLAS-FRLC.
           MOVE TO-LC     TO CLAS-TOLC.
           PERFORM LOAD-CLAS-FILE.
           MOVE CLAS-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              GO TO SETRP.

       RE-SELECT.
           MOVE "PRIOR SELECTION FOR TRANSFER, EXISTS??????" TO MESS.
           PERFORM SEND-MESS.
           MOVE "DO YOU WANT TO RE-SELECT ACCOUNTS? (Y/N)" TO MESS.
           PERFORM SEND-LEGENDX.
           MOVE "E  A010MESSXX." TO VDU.
           PERFORM SCREENIO.
           IF VDUBUF = "1>"
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.
           IF VDUSTATUS NOT = "00"
              GO TO RE-SELECT.
           IF VDUBUF = "N"
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.
           IF VDUBUF NOT = "Y"
              GO TO RE-SELECT.
           MOVE "MESSX MESSXX." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
      *    CALL FRESET USING STAT MESS.
           PERFORM OPEN-WK-FILE-OUTPUT.

       SETRP.
           MOVE " " TO ERRCD.
           MOVE CLAST1-QUEUE-POS TO Q-POS.
           MOVE CLAST1-COPIES-POS TO C-POS.
           MOVE CLAST1-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.

           PERFORM ENTER-PARAMETERS.
           IF ERRCD NOT = " "
              IF ERRCD = "I"
                 GO TO SETRP
              ELSE
                 GO TO INITIALIZATION-EXIT.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE FR-LC TO H-FRLC.
           MOVE "-"   TO H-DASH.
           MOVE FR2-LC TO H-FR2LC.
           MOVE TO-LC TO H-TOLC.
           MOVE TO-DLNO TO H-TO-DLNO.

           MOVE "LOAN SELECT FOR CLASS TRANSFER IN PROGRESS........."
                                                       TO MESS.
           PERFORM SEND-LEGENDX.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

           MOVE 0 TO NOSEL
                     AMTSEL.
           PERFORM ZERO-TT-TOTALS.
           PERFORM ZERO-DD-TOTALS.
           PERFORM ZERO-NM-TOTALS.

       INITIALIZATION-EXIT.
           EXIT.


      ****************************************
       ENTER-PARAMETERS SECTION.
      ****************************************
       ENTER-PARM.

       DEAL.
           MOVE "ENNN042DEALNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10"
              MOVE "I" TO ERRCD
              GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DEAL.
           MOVE VDUNUM2 TO DEALNO.

           MOVE VDUNUM2 TO DL-DLNO
                           TO-DLNO.
           IF TO-DLNO NOT = 0
              PERFORM OPEN-DL1-FILE
              PERFORM READ-DL1-FILE
              PERFORM CLOSE-DL1-FILE
              IF NOT IO-GOOD
                 MOVE "DEALER DOES NOT EXIST!" TO MESS
                 PERFORM SEND-MESS
                 GO TO DEAL
              END-IF
           ELSE
              MOVE "NO CHANGE IN DEALER NO !!!!!!!!!" TO DL-NAME.
           MOVE "S  A030DLNAME." TO VDU.
           MOVE DL-NAME TO VDUBUF.
           PERFORM SCREENIO.

       CHGSPRLC.
           MOVE "E  A010CHGSPRLC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO DEAL.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CHGSPRLC.
           MOVE VDUBUF TO CHG-SPR-LC.
           IF NOT (CHG-SPR-LC = "Y" OR "N")
              MOVE "ENTER Y OR N " TO MESS
              PERFORM SEND-MESS
              GO TO CHGSPRLC.
           IF CHG-SPR-LC = "N"
              MOVE "N" TO BUILD-SPR
              MOVE "S  A220BSPRX." TO VDU
              PERFORM SCREENIO
              MOVE "S  A010BUILDSPR." TO VDU
              PERFORM SCREENIO
              GO TO POOL.

           MOVE "S  A220BSPRX:S." TO VDU.
           MOVE "BUILD NEW SPR RECORDS:" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010BUILDSPR." TO VDU.
           MOVE "_" TO VDUBUF.
           PERFORM SCREENIO.
       BUILDSPR.
           MOVE "E  A010BUILDSPR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO CHGSPRLC.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BUILDSPR.
           MOVE VDUBUF TO BUILD-SPR.
           IF NOT (BUILD-SPR = "Y" OR "N")
              MOVE "ENTER Y OR N " TO MESS
              PERFORM SEND-MESS
              GO TO BUILDSPR.
       POOL.
           MOVE "ENNN050POOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO CHGSPRLC.
           IF VDUSTATUS = "16"
              MOVE "SNNN050POOL." TO VDU
              MOVE 99999 TO VDUNUM0
                            POOLID
              PERFORM SCREENIO
              GO TO BEG-ZIP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO POOL.
           MOVE VDUNUM0 TO POOLID.
           IF POOLID = 0
              GO TO POOL.

       BEG-ZIP.
           MOVE "E  A100BZIP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "16"
              GO TO ZIP-ALL.
           IF VDUSTATUS = "1U" GO TO POOL.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEG-ZIP.
           MOVE VDUBUF TO BEG-ZIPCODE.
       END-ZIP.
           MOVE "E  A100EZIP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO BEG-ZIP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO END-ZIP.
           MOVE VDUBUF TO END-ZIPCODE.
           IF BEG-ZIPCODE > END-ZIPCODE
              PERFORM INVALID-RANGE
              GO TO END-ZIP.
           GO TO BEG-ACCNT.
       ZIP-ALL.
           MOVE "S  A100BZIP." TO VDU.
           MOVE " " TO BEG-ZIPCODE
           PERFORM SCREENIO.
           MOVE "S  A100EZIP." TO VDU.
           MOVE "ZZZZZZZZZZ " TO VDUBUF
                                 END-ZIPCODE
           PERFORM SCREENIO.

       BEG-ACCNT.
           MOVE "ENNN060BEGLN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "16" GO TO ACCNT-PASSWORD.
           IF VDUSTATUS = "1U" GO TO END-ZIP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEG-ACCNT.
           MOVE VDUNUM0 TO BEG-ACCTNO.
       END-ACCNT.
           MOVE "ENNN060ENDLN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO BEG-ACCNT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO END-ACCNT.
           MOVE VDUNUM0 TO END-ACCTNO.
           IF BEG-ACCTNO > END-ACCTNO
              PERFORM INVALID-RANGE
              GO TO END-ACCNT.
           IF BEG-ACCTNO NOT = END-ACCTNO
              PERFORM ACCNT-PASSWORD.
           GO TO BEG-REC.

       ACCNT-PASSWORD.
           MOVE ACCT-SEQ TO PASSWDW-SEQ.
           MOVE "TO SELECT MULTIPLE ACCOUNTS:" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE "MULTIPLE ACCOUNT TRANSFERS NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-ACCNT.

       ACCNT-ALL.
           MOVE "SNNN060BEGLN." TO VDU.
           MOVE 0 TO VDUNUM0
                     BEG-ACCTNO
           PERFORM SCREENIO.
           MOVE "SNNN060ENDLN." TO VDU.
           MOVE 999999 TO VDUNUM0
                          END-ACCTNO
           PERFORM SCREENIO.

       BEG-REC.
           MOVE "ENNN040BREC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "16" GO TO REC-ALL.
           IF VDUSTATUS = "1U" GO TO END-ACCNT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEG-REC.
           MOVE VDUNUM0 TO BEG-RECENCY.
       END-REC.
           MOVE "ENNN040EREC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO BEG-REC.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO END-REC.
           MOVE VDUNUM0 TO END-RECENCY.
           IF BEG-RECENCY > END-RECENCY
              PERFORM INVALID-RANGE
              GO TO END-REC.
           GO TO BEG-CON.
       REC-ALL.
           MOVE "SNNN040BREC." TO VDU.
           MOVE 0 TO VDUNUM0
                     BEG-RECENCY
           PERFORM SCREENIO.
           MOVE "SNNN040EREC." TO VDU.
           MOVE 9999 TO VDUNUM0
                        END-RECENCY
           PERFORM SCREENIO.

       BEG-CON.
           MOVE "ENNN040BCON." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO END-REC.
           IF VDUSTATUS = "16" GO TO CON-ALL.
           IF VDUSTATUS = "1U" GO TO END-REC.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEG-CON.
           MOVE VDUNUM0 TO BEG-CONTRACT.
       END-CON.
           MOVE "ENNN040ECON." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO BEG-CON.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO END-CON.
           MOVE VDUNUM0 TO END-CONTRACT.
           IF BEG-CONTRACT > END-CONTRACT
              PERFORM INVALID-RANGE
              GO TO END-CON.
           GO TO BEG-SUBCLASS.
       CON-ALL.
           MOVE "SNNN040BCON." TO VDU.
           MOVE 0 TO VDUNUM0
                     BEG-CONTRACT.
           PERFORM SCREENIO.
           MOVE "SNNN040ECON." TO VDU.
           MOVE 9999 TO VDUNUM0
                        END-CONTRACT.
           PERFORM SCREENIO.

       BEG-SUBCLASS.
           MOVE "ENNN020BSUBCLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "16" GO TO CLASS-ALL.
           IF VDUSTATUS = "1U" GO TO END-CON.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEG-SUBCLASS.
           MOVE VDUNUM0 TO BEG-SUBCLASS.
       END-SUBCLASS.
           MOVE "ENNN020ESUBCLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO BEG-SUBCLASS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO END-SUBCLASS.
           MOVE VDUNUM0 TO END-SUBCLASS.
           IF BEG-SUBCLASS > END-SUBCLASS
              PERFORM INVALID-RANGE
              GO TO END-SUBCLASS.
           GO TO TRUE-REPO.
       CLASS-ALL.
           MOVE "SNNN020BSUBCLASS." TO VDU.
           MOVE 0 TO VDUNUM0
                     BEG-SUBCLASS.
           PERFORM SCREENIO.
           MOVE "SNNN020ESUBCLASS." TO VDU.
           MOVE 99 TO VDUNUM0
                      END-SUBCLASS.
           PERFORM SCREENIO.

       TRUE-REPO.
           MOVE "ENNA010ONHAND." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO END-SUBCLASS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TRUE-REPO.
           MOVE VDUBUF TO ONHAND.
           IF NOT (ONHAND = "I" OR "S" OR "O")
              GO TO TRUE-REPO.
           IF ONHAND = "O"
              MOVE "S  A010SOLD." TO VDU
              MOVE "S" TO VDUBUF SOLD
              PERFORM SCREENIO
              MOVE "S  A010REDEEM." TO VDU
              MOVE "S" TO VDUBUF REDEEM
              PERFORM SCREENIO
              GO TO PLONLY.

       SOLD-REPO.
           MOVE "ENNA010SOLD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO TRUE-REPO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SOLD-REPO.
           MOVE VDUBUF TO SOLD.
           IF NOT (SOLD = "I" OR "S" OR "O")
              GO TO SOLD-REPO.
           IF SOLD = "O"
              MOVE "S  A010ONHAND." TO VDU
              MOVE "S" TO VDUBUF ONHAND
              PERFORM SCREENIO
              MOVE "S  A010REDEEM." TO VDU
              MOVE "S" TO VDUBUF REDEEM
              PERFORM SCREENIO
              GO TO PLONLY.

       REDEEMED.
           MOVE "ENNA010REDEEM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO SOLD-REPO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REDEEMED.
           MOVE VDUBUF TO REDEEM.
           IF NOT (REDEEM = "I" OR "S" OR "O")
              GO TO REDEEMED.
           IF REDEEM = "O"
              MOVE "S  A010SOLD." TO VDU
              MOVE "S" TO VDUBUF SOLD
              PERFORM SCREENIO
              MOVE "S  A010ONHAND." TO VDU
              MOVE "S" TO VDUBUF ONHAND
              PERFORM SCREENIO
              GO TO PLONLY.

       PLONLY.
           MOVE "E  A010PLONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO REDEEMED.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLONLY.
           MOVE VDUBUF TO PL-ONLY.
           IF NOT (PL-ONLY = "I" OR "S" OR "O")
              GO TO PLONLY.

       ZEROBAL.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO PLONLY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ZEROBAL.
           MOVE VDUBUF TO ZBAL.
           IF NOT (ZBAL = "I" OR "S" OR "O")
              GO TO ZEROBAL.

       BEG-DEALER.
           MOVE "ENNN042BDLNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO ZEROBAL.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEG-DEALER.
           MOVE VDUNUM2 TO BEG-DLNO.
       END-DEALER.
           MOVE "ENNN042EDLNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO BEG-DEALER.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO END-DEALER.
           MOVE VDUNUM2 TO END-DLNO.
           IF BEG-DLNO > END-DLNO
              PERFORM INVALID-RANGE
              GO TO END-DEALER.

       REC-FORM.
           MOVE "ENNN030FORMNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1<"
              PERFORM FORM-SCAN
              IF IO-BAD
                 GO TO REC-FORM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "1U" GO TO END-DEALER.
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
           IF POOLID = 99999
            IF BEG-ZIPCODE = " "
             IF END-ZIPCODE = "ZZZZZZZZZZ"
              IF BEG-ACCTNO = 0
               IF END-ACCTNO = 999999
                IF BEG-RECENCY = 0
                 IF END-RECENCY = 9999
                  IF BEG-CONTRACT = 0
                   IF END-CONTRACT = 9999
                    IF BEG-SUBCLASS = 0
                     IF END-SUBCLASS = 99
                      IF BEG-DLNO = 0
                       IF END-DLNO = 9999.99
                        IF PL-ONLY = "I" AND ZBAL = "I"
                 MOVE "NOTE: YOU WILL TRANSFER ALL ACCOUNTS!"
                                                          TO MESS
                           PERFORM SEND-MESS
                        ELSE
                        IF PL-ONLY = "S" AND ZBAL = "S"
           MOVE "NOTE: YOU WILL TRANSFER ALL ACTIVE, NON P&&L ACCOUNTS!"
                                                          TO MESS
                           PERFORM SEND-MESS
                        ELSE
                        IF PL-ONLY = "O" AND ZBAL = "S"
           MOVE "NOTE: YOU WILL TRANSFER ALL NON ZERO, P&&L ACCOUNTS!"
                                                          TO MESS
                           PERFORM SEND-MESS.

           MOVE "E RA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO DEAL.
           IF VDUSTATUS = "1U" GO TO REC-FORM.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-IT.
           IF NOT (VDUBUF = "Y"  OR "N") GO TO ACCEPT-IT.
           IF VDUBUF = "N"
              GO TO DEAL.
           GO TO EXIT-PARM.

       EXIT-PARM-ABORT.
           MOVE "X" TO ERRCD.

       EXIT-PARM.
           EXIT.

      ****************************************
       FORM-SCAN SECTION.
      ****************************************
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


      ****************************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
               BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO HOLD-LPS-NUM.
           MOVE H-LPS-NUM TO D-LOAN-NUM.
           MOVE LN-1STPYDA TO D-DUEDAY.
           MOVE LN-DLNO TO D-DLNO.
           IF LN-DATE-PAID-LAST NOT = 0
              MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO D-PAYDATE
           ELSE
              MOVE LN-LOANDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO D-PAYDATE
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
           MOVE 0 TO UNEARN-INT UNEARN-SC UNEARN-MF.
           MOVE TRANS-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.

           PERFORM COMPUTE-EARNINGS.
           MOVE EARN-UNEARN(1) TO UNEARN-INT.
           MOVE EARN-UNEARN(3) TO UNEARN-SC.
           MOVE EARN-UNEARN(4) TO UNEARN-MF.

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
           MOVE PDTH-DATE TO D-PDTHRU.
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
       END-DETAIL-LN-PROCESS.
           EXIT.

      ****************************
       DETAIL-LN-ACCTBL SECTION.
      ****************************
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
           ADD UNEARN-INT TO T-UNE(LEV SAVE-PSUB 3).
           ADD UNEARN-SC TO T-IER(LEV SAVE-PSUB 3).
           ADD UNEARN-MF TO T-ERN(LEV SAVE-PSUB 3).

       DETAIL-LN-ACCTBL-EXIT.
           EXIT.

      ****************************************
       VERIFY-GI SECTION.
      ****************************************
           PERFORM OPEN-GI1-FILE.
           MOVE "Y" TO VALID-REC-FG.
           MOVE LN-OWNBR TO GI-BRANCH.
           MOVE TO-LC TO GI-CLASS.
           PERFORM READ-GI1-FILE.
           IF IO-BAD
              MOVE "N" TO VALID-REC-FG.
           PERFORM CLOSE-GI1-FILE.

      **************************
       VERIFY-SPR SECTION.
      **************************
           MOVE "Y" TO VALID-REC-FG.
           MOVE LN-ORGST    TO LPWSSP-ORGST.
           MOVE TO-LC       TO LPWSSP-SPRCLASS.
           MOVE LN-SUBCLASS TO LPWSSP-SUBCLASS.
           MOVE LN-LAWCODE  TO LPWSSP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE LPWSSP-SP1-KEY TO SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-BAD
                 MOVE "N" TO VALID-REC-FG.

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

           MOVE "BEG. FROM CLASS    :" TO RANGE-LINE.
           MOVE FR-LC                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END  FROM CLASS    :" TO RANGE-LINE.
           MOVE FR2-LC               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TO CLASS           :" TO RANGE-LINE.
           MOVE TO-LC                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEALER NUMBER      :" TO RANGE-LINE.
           MOVE DEALNO TO RANGE-DEALER.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "POOL ID            :" TO RANGE-LINE.
           MOVE POOLID TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING ZIP CODE" TO RANGE-LINE.
           MOVE BEG-ZIPCODE          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING ZIP CODE    :" TO RANGE-LINE.
           MOVE END-ZIPCODE            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING ACCOUNT  :" TO RANGE-LINE.
           MOVE BEG-ACCTNO             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING ACCOUNT     :" TO RANGE-LINE.
           MOVE END-ACCTNO             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING RECENCY  :" TO RANGE-LINE.
           MOVE BEG-RECENCY            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING RECENCY     :" TO RANGE-LINE.
           MOVE END-RECENCY            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CONTRACT :" TO RANGE-LINE.
           MOVE BEG-CONTRACT           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING CONTRACT    :" TO RANGE-LINE.
           MOVE END-CONTRACT           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SUB CLASS:" TO RANGE-LINE.
           MOVE BEG-SUBCLASS           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING SUB CLASS   :" TO RANGE-LINE.
           MOVE END-SUBCLASS           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPO   ONHAND      :" TO RANGE-LINE.
           MOVE ONHAND                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "REPO   SOLD        :" TO RANGE-LINE.
           MOVE SOLD                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "REPO   REDEEM      :" TO RANGE-LINE.
           MOVE REDEEM                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L ACCOUNTS ONLY  :" TO RANGE-LINE.
           MOVE PL-ONLY                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ZERO BALANCE ACCTS :" TO RANGE-LINE.
           MOVE ZBAL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DEALER   :" TO RANGE-LINE.
           MOVE BEG-DLNO               TO RANGE-DEALER.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING DEALER      :" TO RANGE-LINE.
           MOVE END-DLNO               TO RANGE-DEALER.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FORM REQUEST NO.   :" TO RANGE-LINE.
           MOVE FORMNO                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.


      ***************************************
       SET-BRANCH-HEADINGS SECTION.
      ***************************************

           MOVE HOLD-BRNO TO BR-NO H-BRNO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           MOVE BR-NAME TO H-BRDESC.
           PERFORM CLOSE-BR-FILE.

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

      ***************************
       PRINT-DETAIL-SUM SECTION.
      ***************************

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


      *****************************************
       CREATE-FM-REC SECTION.
      *****************************************

           IF LN-CONFIDENTCD = "Y" OR BW-CONFIDENTIAL = "Y"
              GO TO END-CREATE-FM-REC.

           PERFORM OPEN-FM1-FILE.

           PERFORM CLEAR-FM-FILE.

           MOVE SPACES        TO LPWSFM-EDIT-FM-INITKEY.

           IF FD-1STFILE = 2
              MOVE LN-SSNO(1) TO LPWSFM-EDIT-FM-SSNO
           ELSE
              MOVE LN-ACCTNO  TO LPWSFM-EDIT-FM-ACCTNO
              MOVE FORMNO     TO LPWSFM-EDIT-FM-FORMNO.

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

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "CLASTR" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPFMCL.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO CLAST1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLAST1-SYSDATE.
           DISPLAY CLAST1-SCREEN UPON CLAST1-WINDOW-HANDLE.
           MOVE CLAST1-FORM-FIELDS TO WR-BUF.
           MOVE CLAST1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE CLAST1-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLAST1-HELP--DIR  TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CLAST1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************
       MISC-ROUTINES SECTION.
      *****************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
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

       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-LEGENDX.
           MOVE MESS TO WR-BUF.
           MOVE MESSX-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-SP.
           MOVE LN-ORGST    TO LPWSSP-ORGST.
           MOVE LN-CLASS    TO LPWSSP-SPRCLASS.
           MOVE LN-SUBCLASS TO LPWSSP-SUBCLASS.
           MOVE LN-LAWCODE  TO LPWSSP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE LPWSSP-SP1-KEY TO SP1-KEY
              PERFORM READ-SP1-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.

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

      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPIOCLAS.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBLP/LPWKI.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CLAST1-SCREEN.
           DESTROY CLAST1-WINDOW-HANDLE.
           EXIT PROGRAM.
