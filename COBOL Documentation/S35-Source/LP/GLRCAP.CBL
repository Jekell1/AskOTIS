       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GLRCAP.
       DATE-WRITTEN.  020900
      ******************************************************************
      *   DIRECTORY  :  LP
      *   DESCRIPTION:  EOD GL DETAIL BY TRANSACTION
      *
      *   TO DO PR#2001, ADD ACCT# RANGE & CK# RANGE:
      *   IF TR1-CODE NOT = D OR W OR C
      *      CHECK ACCT# RANGE
      *   IF TR1-CODE =  C
      *      CHECK CK# RANGE, EXPLAIN IN HELP THAT CASH IS 0
      *   ALSO EXPLAIN IN HELP, IF THEY WANT TO SEE ONLY LOAN # WHATEVER,
      *   PUT IN A CK RANGE OF 999999-999999 OR SOMETHING.  ONLY CK
      *   PUT IN ACCT RANGE OF 0-0
      *
      *
      *
      *   R E M E M B E R:    CHANGE LP/LPEOD5.C & LP/RECAPJ.C
      * REV:
      *  MJD 000602 SUPPRESS PRINTING OF CKWRITTEN(4) IF = 0
      *  JTG 000613 CHANGED UPDATE TEST ON GP-LOAN-CONVERSION-UPD-FG
      *             TO ALWAYS CREATE A TRK TRANSACTION RECORD         #1203
      *             AND TO TEST WHETHER TO CREATE A GT RECORD
      *  BLV 000627 CHANGE ED-KEY TO INCLUDE TR-SUBKEY, SO GL DETAIL WILL
      *             BE BROKEN OUT WHEN THERE IS MORE THAN 1 TRANSACTION
      *             OF THE SAME KIND FOR THE SAME ACCOUNT IN THE SAME DAY
      *  BAH 010319 ADDED BRANCH SECURITY PASSWORD TESTING (BRSECURE)
      *  CS  010810 ADDED OPTION FOR OUTPUT FILE(MERCURY PR#254) PER KEVIN,
      *             DONT PRODUCE DETAIL, JUST A TOTAL OF RECORDS WRITTEN
      *  JTG 021121 ADDED NEW CODE FILE TYPE 'BR' -BATCH PAYMENT REFERENCE CODES
      *             BATCH PAYMENT REVISIONS                         REGACC #0006
      *  BLV 030124 ADDED RANGE ON LOAN ACCT#, CHECK #; ADDED FLAG WHETHER
      *             TO INCLUDE DEPOSITS.  AS FAR AS I CAN TELL, THERE ARE
      *             DEPOSIT TR'S, THEN CASH/CHECK TR'S, AND ALL OTHER REDEFS
      *             SHOULD HAVE THE LOAN # IN THE SAME SPOT.  WILL ADD STUFF
      *             TO HELP EXPLAINING HOW, IF YOU WANT TO SEE ALL TRANS FOR
      *             A LOAN, SAY ALL TRANS TYPES, ENTER LOAN ACCT# IN THAT
      *             RANGE, SAY CHECK # 999999-999999 (TO SKIP CASH WHICH IS
      *             CK NO OF 0) & SAY "N" TO INCLUDE DEPOSITS.  LIKEWISE, YOU
      *             COULD ENTER A BOGUS LOAN # RANGE IF YOU'RE JUST TRYING
      *             TO GET ALL THE TRANS FOR A CERTAIN CHECK NUMBER. PR# 2001
      *  GLB 030508 WORLD PR#323 ADDED NEW "CT-FILE" FOR LIBLP/LPEOD5TR
      *             CODE TRAILER G/L INTERFACE TABLE & AMOUNT
      *             ONLY OCCURS IF CD-GC-FEE-GIFTNET = "Y"
      *  CS  031111 ADDED FEE-COMMISSION [WORLD PR# 331]
      * JTG 040404 ADDED LOGIC FOR POSTING BATCH PAYMENTS TO ACCOUNT'S
      *            WITH 'LN-OTBAL'                                 CENTRIX #2253
      *            ADDED BATCH-REFCD-OT-GLNO
      * BLF 070822 BARB & CINDY.  IF I-AM-WORLD & PMT POSTED BY COLLECTION
      *            BRANCH (BR# 197) ON P&L ACCOUNT, ALTER GL UPDATE SO POSTING
      *            BRANCH GETS P&L RECOVERIES POSTED TO THEIR BRANCH,
      *            WORLD PR# 551
      * BLF 2007.0830 UNDID WORLD PR# 551
      *  CS 2008.1216 ADDED BR-FILE. THE BR-USE-PARTIC-FLAT-TABLE IS TESTED IN
      *               LPEOD5TR  PR#232
      * BAH 2011.0411 UNCOMP'D CKWRITTEN, MEMORY ACCESS VIOLATION IF REALLY
      *               LARGE NUMBERS, PERF EQ PL# 735
      *  CS 2015.0819 ADD BRANCH-COMMISSION UESD IN LPEOD5TR EXCEL PR#4821
      *  CS 2015.0922 CHANGED BRANCH-COMMISSION TO S9(6) PR#4821 EXCEL
      * KEC 2015.1120 FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      * BLM 2016.0224 FIX FOR NEW TY-DATE FORMAT
      * KEC 2016.0315 CHANGED CD-FEE???? TO CD-GC-????.
      * BLM 2016.0726 IF GP-I-AM-WORLD & CD-COMM-PERCENT = 99.99, FORCE THE
      *               PAYABLE PERCENT TO 100, FOR LIVE CHECKS, WORLD PR# 1170
      * KEC 2017.0109 REMOVED TRANS-DATE-SETTT [PIC 9(7)] SINCE NOT USED.
      * BAH 2017.0605 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0801 CHANGED DGL-AMOUNT PIC S9(9)V99 SIGN TRAILING SEPARATE
      *               TO PIC +9(9).99 WITH MAKING "+" TO A SPACE.
      *               ADDED DGL-REFNO PIC X(5).
      *               CHANGED DGL-REC REC SIZE INCREASED BY 6 (50 BYTES PER REC)
      *               ADDED TO THE EOD WORK FILE (ED-REC):
      *               ED-TP-REFNO  PIC X(5);
      *               ED-TP-PLCD   PIC X(1).
      *               CHANGED DETAIL1 FOR D-G-DESCRIPTION PIC X(36) TO X(28);
      *               ADDED D-TR-REFNO X(5) AND D-TR-PLCD X(1).       #1262
      * KEC 2017.0824 PR#1262 EMAIL FROM KEVIN MULLEN, CHANGED THE FOLLOWING:
      *         - USE TP-PLTYPE INSTEAD OF TP-PLCD.
      *         - ADD TP-PLTYPE TO THE END OF THE EXTRACTION RECORD.
      *         - MOVE TP-PLTYPE BETWEEN REFNO AND SOURCE ON REPORT.
      * BAH 2018.1018 GLOBAL TYFILE
      *  CS 2018.1213 IF WORLD, CHANGE BANK TRANSACTIONS ID, IW, & BK TO
      *               UPDATE TO DEPOSIT BUCKET IN ED-FILE, SO THEY WILL
      *               BE UPDATED TO GTFILE WITH EXPLANATION OF "EOD-DEPOSIT",
      *               WORLD PR# 1353
      *               RECAPJ,GLRCAP,GLDIST,CL/CK1497 WERE SKIPPED IN
      *               ORIGINAL CHANGE
      * BAH 2019.0927 REMOVED ACCESS-CALL TRB, TRC, TRD, TRJ, TRK, TRL,
      *               TRP, TRS, TRX
      * BAH 2020.0625 ADDED OPEN-CDB1-FILE, OPEN-CD1-FILE, OPEN-LC1-FILE,
      *               OPEN-GI1-FILE, REMOVED THEM FROM LPEOD5TR, #TST001
      * BAH 2021.0422 ADDED TR-POSTING-BR AND TR-OTHBR-FG #1496
      * BLM 2021.0810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * BAH 2021.0531 ADDED DGL-ACTIONCD2 TO EXTRACT FILE FOR TP-ACTIONCD2,
      *               TL-ACTIONCD2, TB-ACTIONCD2, TS-ACTIONCD2; ADDED
      *               ACTIONCD2 RANGE, SECURITIZATION #1555
      * MB  2023.1221 CHANGES DUE TO NEW ARRAY PROCESING IN LPEOD5TR.CPY
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSED.CPY".
       COPY "LIBLP/LPFSER.CPY".
       COPY "LIBGB/GBFSPR.CPY".
       SELECT DGL-FILE ASSIGN DGL-PATH
           ORGANIZATION RELATIVE
           ACCESS DYNAMIC
           RELATIVE KEY DGL-KEY
           FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      **************************************************
      *      E.O.D. REPORT WORK FILE                   *
      *      (EACH REPORT TO DEFINE ED-REC)            *
      **************************************************
       FD  ED-FILE
           LABEL RECORDS ARE STANDARD.
      **************************************************
      *      E.O.D. REPORT WORK FILE                   *
      *      (EACH REPORT TO DEFINE ED-REC)            *
      * REV:
      * BLV 091097 PUT FIELDS IN THIS COPY MEMBER
      **************************************************
       01  ED-REC.
           03  ED-KEY.
               05  ED-CODE-ACCT.
                   07  ED-TR-CODE     PIC X.
                   07  ED-TR-ACCT     PIC 9(8).
               05  ED-TR-SUBKEY       PIC X(17).
               05  ED-TR-SEQNO        PIC 9(3).
               05  ED-NO              PIC 9(11).

           03  ED-ID                  PIC X.
           03  ED-AMT  OCCURS 24      PIC S9(9)V99 COMP-3.
           03  ED-TR-TYPE             PIC XX.
           03  ED-TP-REFNO            PIC X(5).
           03  ED-TP-PLTYPE           PIC X(1).
           03  ED-UPDATED             PIC X.

      * ED-AMT(2)  - TRANSACTION CODES (TRP)
      * ED-AMT(3)  - SALES FINANCE LOANS (TRS)
      * ED-AMT(4)  - DIRECT LOANS (TRL)
      * ED-AMT(5)  - TRANSFERS (TRX)
      * ED-AMT(6)  - CHECKS (TRC)
      * ED-AMT(7)  - DEPOSITS (TRD)
      * ED-AMT(8)  - BRANCH (BULK) TRANSFERS (TRB)
      * ED-AMT(9)  - BRANCH (CLASS) TRANSFERS (TRJ)
      * ED-AMT(10) - BRANCH (CLASS) TRANSFERS (TRJ)

      * RECORD SIZE = 60
       FD  DGL-FILE
           LABEL RECORDS ARE STANDARD.
       01  DGL-REC.
           03  DGL-BRANCH                 PIC 9(4).
           03  DGL-CODE                   PIC X.
           03  DGL-CHK-ACCTNO             PIC 9(6).
           03  DGL-ACCT-GL                PIC 9(11).
           03  DGL-AMOUNT                 PIC +9(9).99.
           03  DGL-DATE-MMDDYY            PIC 9(6).
           03  DGL-TRCD                   PIC X(2).
           03  DGL-REFNO                  PIC X(5).
           03  DGL-PLTYPE                 PIC X(1).
           03  DGL-POSTING-BR             PIC 9(4).
           03  DGL-OTHBR-FG               PIC X(1).
           03  DGL-ACTIONCD2              PIC X(4).
           03  WK-CR                      PIC X.
           03  WK-LF                      PIC X.


      *DGL-CODE

      *B = BULK (BRANCH) TRANSFER
      *C = CASH/CHECK
      *D = DEPOSIT
      *E = REVOLVING SALES FINANCE
      *F = DUMMY POOL RECORDS
      *G = SET POOL RECORD
      *J = CLASS TRANSFERS
      *K = CONVERSION LOANS
      *L = LOAN
      *P = PAYMENT
      *S = SALES FINANCE
      *W = CASH BALANCING RECORDS
      *X = PURCHASE/SERVICE


       COPY "LIBLP/LPFDER.CPY".
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/GLRCAP.CBL S35 04/22/25-12:14:48 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRX.CPY".
       COPY "LIBLP/LP01TRX_SQL.CPY".
       COPY "LIBLP/LPWSTRX.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LP01TRJ_SQL.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
       COPY "LIBLP/LPWSER.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBGB/ARRAYBRW.CPY".
       COPY "LIBLP/ARRAYLCW.CPY".
       COPY "LIBGL/ARRAYGIW.CPY".

      *-----------------------------------------------------------------
      *  D G L  -  P A T H === /USR/DATA/R#/SP/GLRCAP.###### (MMDDYY)
      *-----------------------------------------------------------------
       01  DGL-PATH.
           03  DGL-PATH-BASE           PIC X(09).
           03  FILLER                  REDEFINES DGL-PATH-BASE.
               05  DGL-PATH-BASE-USR   PIC X(05).
               05  DGL-PATH-BASE-DIR   PIC X(04).
           03  FILLER                  PIC X(01)   VALUE "/".
           03  DGL-PATH-MACHINE        PIC X(02).
           03  FILLER                  PIC X(11)   VALUE "/SP/GLRCAP.".
           03  DGL-PATH-DATE           PIC 9(06).

       01  ED1-IFN              PIC X(3)     VALUE "ED1".
       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  DGL-KEY              PIC 9(7)     VALUE 0.

      * BASE ELEMENT NUMBERS FOR UNEARNED AND EARNINGS
      * GL ACCOUNTS:

       01  FUNK-SUB             PIC 999.
      *PLSUB MUST BE PIC 9:
       01  PLSUBX.
           03  PLSUB            PIC 9.

       01  SAVE-ED-TR-SEQNO     PIC 9(3).
       01  SAVE-ED-CODE-ACCT    PIC X(9).
       01  SAVE-ED-TR-SUBKEY    PIC X(17).

       01  RECORDS-WRITTEN  PIC 9(9).
       01  TOTAL-CREDITS    PIC S9(9)V99
                     SIGN TRAILING SEPARATE.
       01  TOTAL-DEBITS     PIC S9(9)V99
                     SIGN TRAILING SEPARATE.
       01  FIRST-TIME-ENTRY  PIC X.
       01  FEE-COMMISSION    PIC S9(4)V99.

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.
       01  LEGEND               PIC X(60)    VALUE SPACES.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  EOD-DATE        PIC 9(8).
           03  FILLER REDEFINES EOD-DATE.
               05  EOD-CC      PIC 99.
               05  EOD-YR      PIC 99.
               05  EOD-MO      PIC 99.
               05  EOD-DA      PIC 99.
           03  EOD-DATE2       PIC 9(8).
           03  FILLER REDEFINES EOD-DATE2.
               05  EOD-CC2     PIC 99.
               05  EOD-YR2     PIC 99.
               05  EOD-MO2     PIC 99.
               05  EOD-DA2     PIC 99.

           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  GLBR1           PIC 9(4).
           03  GLBR2           PIC 9(4).
           03  ACCOUNT1        PIC 9(7).
           03  ACCOUNT2        PIC 9(7).
           03  TRANS-TYPE      PIC XXX.
           03  OUTPUT-FILE     PIC X.
           03  LOAN1           PIC 9(8).
           03  LOAN2           PIC 9(8).
           03  CHECK1          PIC 9(6).
           03  CHECK2          PIC 9(6).
           03  INCL-DEPOSIT-FG PIC X.
           03  ACTION1         PIC X(4).
           03  ACTION2         PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "B".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6) COMP.
       01  L1                   PIC 9(6) COMP.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  CLEARING-WK          PIC S9(10)V99  COMP.

       01  HOLD-BRNO            PIC 9(4).
       01  DF-TEST              PIC X        VALUE " ".
           88  DF-POSTING                    VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  TOT-DR               PIC S9(9)V99 COMP VALUE 0.
       01  TOT-CR               PIC S9(9)V99 COMP VALUE 0.
       01  GRAND-TOT-DR         PIC S9(9)V99 COMP VALUE 0.
       01  GRAND-TOT-CR         PIC S9(9)V99 COMP VALUE 0.
      * LPEOD5TR
       01  TR-DATE              PIC 9(8).
       01  TR-CODE              PIC X.
       01  TR-ACCT              PIC 9(8).
       01  TR-TYPE-TRCD         PIC XX.
       01  TR-REASON            PIC X(30).
       01  TR-CLASS             PIC 9(3).
       01  TR-REFNO             PIC X(5).  
       01  TR-PLTYPE            PIC X(1).  
       01  TR-POSTING-BR        PIC 9(4).
       01  TR-OTHBR-FG          PIC X(1).
       01  TR-ACTIONCD2         PIC X(4).

       01  TR-SUBKEY            PIC X(17).
       01  TRP-SUBKEY REDEFINES TR-SUBKEY.
           03  TRP-SUBKEY-ACCTNO       PIC 9(8).
           03  TRP-SUBKEY-LINE         PIC 9(3).
       01  OTHER-SUBKEY REDEFINES TR-SUBKEY.
           03  OTHER-SUBKEY-ACCTNO       PIC 9(8).
           03  OTHER-SUBKEY-LAWCODE      PIC 9(2).
           03  OTHER-SUBKEY-LINE         PIC 9(3).

       01  BATCH-REFCD          PIC X(5)      VALUE LOW-VALUES.
       01  BATCH-REFCD-GLNO     PIC 9(11)     VALUE 0.
       01  BATCH-REFCD-OT-GLNO  PIC 9(11)     VALUE 0.
       01  GLNO                 PIC 9(11)     VALUE 0.
       01  GLNOX               REDEFINES GLNO.
           03  FILLER           PIC X(4).
           03  FILLER           PIC X(3).
           03  GLOTHBR          PIC 9999.
       01  GLNOZ               REDEFINES GLNO.
           03  GLNO-BR          PIC 9999.
           03  GLNO-NUMBER      PIC 9(7).
       01  FILLER               REDEFINES GLNO.
           03  GLNO-BRANCH      PIC 9(4).
           03  GLNO-ACCT        PIC 9(7).

       01  GLAMT                PIC S9(9)V99 VALUE 0.
       01  GLWK                 PIC S9(9)V99 VALUE 0.
       01  HOLD-PL-EXP          PIC S9(9)V99 VALUE 0.
       01  ERROR-FG             PIC X VALUE "N".

       01  HOLD-COMM-PERCENT    PIC 999V99 VALUE 0.


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
           03  H-TITLE          PIC X(41).

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC ZZ99.
           03  FILLER           PIC X VALUE " ".
           03  FILLER           PIC X(43) VALUE " ".
           03  H-TRDATE         PIC 99/99/99.
           03  FILLER           PIC X(6) VALUE " THRU ".
           03  H-TRDATE2        PIC 99/99/99.

       01  HEAD4.
           03  FILLER           PIC X(44) VALUE
           "DRAWER/DESCRIPTION-------     TIME      TIME".
           03  FILLER           PIC X(44) VALUE
           "  BALANCED     OPENING   NET-CASH CASH-NOTES".
           03  FILLER           PIC X(44) VALUE
           "  ESTIMATED       CASH     ACTUAL           ".

       01  HEAD5.
           03  FILLER           PIC X(44) VALUE
           "  #                         OPENED    CLOSED".
           03  FILLER           PIC X(44) VALUE
           "  BY           BALANCE   ACTIVITY   ACTIVITY".
           03  FILLER           PIC X(44) VALUE
           "    END-BAL      OV/SH    END-BAL    DEPOSIT".

       01  HEAD6.
           03  FILLER           PIC X(13)   VALUE "TRANS TYPE".
           03  FILLER           PIC X(11)   VALUE "ACCT/CK #".
           03  FILLER           PIC X(49)   VALUE
           "G/L ACCOUNT  DESCRIPTION                   REFNO ".
           03  FILLER           PIC X(36)   VALUE
           "  SOURCE                         ".
           03  FILLER           PIC X(14)   VALUE "DEBIT".
           03  FILLER           PIC X(7)    VALUE "CREDIT".

       01  SAVE-HEAD4           PIC X(132).
       01  SAVE-HEAD5           PIC X(132).

       01  DETAIL-LINE          PIC X(132) VALUE " ".

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-TR-DESC        PIC X(12).
           03  D-TR-CODE2       PIC X(3).
           03  D-TR-ACCT        PIC ZZZZZ9.
           03  FILLER           PIC XX.
           03  D-G-BRNO         PIC Z9(3).
           03  D-G-HYPHEN       PIC X.
           03  D-G-GLNO         PIC 9(7).
           03  FILLER           PIC X.
           03  D-G-POST         PIC X.
           03  FILLER           PIC X.
           03  D-G-DESCRIPTION  PIC X(28).
           03  FILLER           PIC X(1).
           03  D-TR-REFNO       PIC X(5).
           03  FILLER           PIC X(1).
           03  D-TR-PLTYPE      PIC X(1).
           03  FILLER           PIC X.
           03  D-SOURCE-DESC    PIC X(27).
           03  D-G-DR           PIC ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  D-G-CR           PIC ZZZ,ZZZ,ZZ9.99- BLANK ZERO.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUMBER.
                   05 FILLER       PIC X(5).
                   05 R-NUM        PIC 9(8).
               04  R-DTE REDEFINES R-NUMBER.
                   05 FILLER       PIC X(3).
                   05 R-DATE       PIC 9999/99/99.
               04  R-ALPHABET REDEFINES  R-NUMBER.
                   05 FILLER       PIC X(5).
                   05 R-ALPHA      PIC X(8).
               04  R-DECIMAL REDEFINES R-NUMBER
                                 PIC ZZZZZZZZ9.99-.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBLP/EMSFRG_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/GLRCAP_DEF.CPY".
       COPY "LIBLP/GLRCAP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/GLRCAP_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               ED-FILE PR-FILE DGL-FILE ER-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRX1-FILE.
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRK1-FILE.
           PERFORM CLOSE-TRJ1-FILE.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-TRB1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               ED-FILE DGL-FILE ER-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ***********************************************************
       MAIN-MODULE SECTION.
      ***********************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GLRCAP" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO HOLD-BRNO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
      * NOTE THIS BRANCH IO LOOKS A LITTLE WIERD - IT'S BECAUSE COPY
      * MEMBER LPEOD5TR READS & CLOSES BR-FILE...SO WE HAVE TO SAVE
      * BRNO, REOPEN FILE, START & DO AN EXTRA READ NEXT
       NEXT-GROUP-LOC.
           PERFORM OPEN-BR-FILE.
           IF GROUP-FLAG NOT = "Y"
              GO TO START-NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              PERFORM PRINT-GRAND-TOTALS
              GO TO END-PROGRAM.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.
       START-NEXT-BRANCH-NO.
           MOVE HOLD-BRNO TO BR-NO.

           PERFORM START-BR-FILE.
           PERFORM READ-BR-FILE-NEXT.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              PERFORM PRINT-GRAND-TOTALS
              GO TO END-PROGRAM.

           MOVE BR-NO TO HOLD-BRNO.
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO TRL-PATH-OVERRIDE
                                              TRS-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE
                                              TRX-PATH-OVERRIDE
                                              TRC-PATH-OVERRIDE
                                              TRD-PATH-OVERRIDE
                                              TRB-PATH-OVERRIDE
                                              TRJ-PATH-OVERRIDE
                                              TRK-PATH-OVERRIDE.

      * RELOAD GI TABLE AT BRANCH BREAKS.
           MOVE BR-NO TO ARRAYGI-GI-BRANCH.
           PERFORM ARRAYGI-LOAD.

           MOVE BR-NO TO H-BRNO.
           MOVE "  GENERAL LEDGER DETAIL BY TRANSACTION  " TO H-TITLE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE "GL DETAIL BY TRANSACTION IN PROGRESS.." TO MESS
                 PERFORM PROGRESS-MSG.

           MOVE " " TO HEAD4 HEAD5.
           MOVE HEAD6 TO HEAD4.
           MOVE SPACES TO HEAD5.
           MOVE 99 TO LN-COUNT.

           IF TRANS-TYPE = "ALL" OR "B"
              PERFORM PROCESS-TRB-FILE.
           IF TRANS-TYPE = "ALL" OR "C"
              PERFORM PROCESS-TRC-FILE.
           IF TRANS-TYPE = "ALL" OR "D"
      * ADDED SEPARATE FLAG FOR DEPOSITS, I THINK IT'S THE ONLY TYPE
      * THAT AFFECTS GL BUT DOESN'T HAVE AN ACCT OR CHECK NUMBER:
              IF INCL-DEPOSIT-FG = "Y"
                 PERFORM PROCESS-TRD-FILE.
           IF TRANS-TYPE = "ALL" OR "J"
              PERFORM PROCESS-TRJ-FILE.
           IF TRANS-TYPE = "ALL" OR "K"
              IF (GP-LOAN-CONVERSION-UPD-FG = "Y" OR "G")
                 PERFORM PROCESS-TRK-FILE.
      * SKIP CONVERSIONS UNLESS GP SAYS TO AFFECT GL:
           IF TRANS-TYPE = "ALL" OR "L"
              PERFORM PROCESS-TRL-FILE.
           IF TRANS-TYPE = "ALL" OR "P"
              PERFORM PROCESS-TRP-FILE.
           IF TRANS-TYPE = "ALL" OR "S"
              PERFORM PROCESS-TRS-FILE.
           IF TRANS-TYPE = "ALL" OR "X"
              PERFORM PROCESS-TRX-FILE.

       GL-SUMMARY.

           IF ERROR-FG = "Y"
              GO TO TODAYS-SUMMARY.

           PERFORM CLOSE-ED1-FILE.

           PERFORM OPEN-ED1-FILE.
           MOVE SPACES TO ED-KEY SAVE-ED-CODE-ACCT SAVE-ED-TR-SUBKEY.
           MOVE 0 TO ED-NO ED-TR-ACCT ED-TR-SEQNO.
           PERFORM START-ED1-FILE.
       NEXT-GL-SUMMARY.
           PERFORM READ-ED1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-GL-SUMMARY.

           PERFORM PRINT-GL-DETAIL.

           GO TO NEXT-GL-SUMMARY.
       END-GL-SUMMARY.
           IF OUTPUT-FILE = "N"
              PERFORM PRINT-GL-TOTALS.

       TODAYS-SUMMARY.
           IF ERROR-FG = "Y"
              MOVE "E" TO EXT-EOBUF-ERRCD.

           PERFORM CLOSE-ED1-FILE.
           PERFORM OPEN-ED1-FILE-OUTPUT.
           PERFORM CLOSE-ED1-FILE.
           PERFORM OPEN-ED1-FILE.
           MOVE SAVE-HEAD4 TO HEAD4.
           MOVE SAVE-HEAD5 TO HEAD5.
           MOVE 99 TO LN-COUNT.

           GO TO NEXT-GROUP-LOC.


      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           MOVE "Y" TO FIRST-TIME-ENTRY.

           MOVE GLRCAP-QUEUE-POS TO Q-POS.
           MOVE GLRCAP-COPIES-POS TO C-POS.
           MOVE GLRCAP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO ED-PATH.
           OPEN OUTPUT ED-FILE.
           IF FILE-STAT NOT = "00"
              MOVE "CAN'T CREATE JOURNAL WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.
           CLOSE ED-FILE.

           IF OUTPUT-FILE = "Y"
              OPEN OUTPUT DGL-FILE
              CLOSE DGL-FILE.

      * GET GLOBAL PARAMETER RECORD (FROM GPENV):
           PERFORM GET-GPENV.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EOD-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-TRDATE.
           MOVE EOD-DATE2 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-TRDATE2.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROGRAM IN PROGRESS..............." TO MESS
              PERFORM PROGRESS-MSG.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-ED1-FILE.

           IF OUTPUT-FILE = "Y"
              PERFORM OPEN-DGL-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-GI1-FILE.

           MOVE ZERO TO RECORDS-WRITTEN
                        TOTAL-CREDITS
                        TOTAL-DEBITS.

      * THESE FOR LPEOD5TR.CPY  - CHANGE MB 2023-1221
           PERFORM ARRAYBR-LOAD.
           PERFORM ARRAYLC-LOAD.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ********************************************
       EO-EXEC-AUDIT SECTION.
      ********************************************
           MOVE EXT-FILPATH-BASE    TO DGL-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO DGL-PATH-MACHINE.
           MOVE EXT-EOBUF-DATE1     TO EOD-DATE.
           MOVE EXT-EOBUF-DATE2     TO EOD-DATE2 DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO DGL-PATH-DATE.


      ********************************************
       ENTER-PARAMETERS SECTION.
      ********************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO MAKE-OUTFILE.

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
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
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-01.

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

       ENTER-DATE.
           IF EXT-ROUTE-BUF NOT = "00" GO TO E-GL-BR1.
           MOVE LEGEND-TRANS-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-DATE.
           MOVE VDU-DATE-YYYYMMDD TO EOD-DATE.

           IF EOD-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD 
              MOVE "S  8000DATE1." TO VDU
              MOVE DATE-YYYYMMDD TO EOD-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

       ENTER-DATE2.
           MOVE "F7-EXIT, ^, V, ENTER ENDING DATE" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-DATE.
           MOVE VDU-DATE-YYYYMMDD TO EOD-DATE2.

           IF EOD-DATE2 = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000DATE2." TO VDU
              MOVE DATE-YYYYMMDD TO EOD-DATE2 VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

      *VALID DATE RANGE
           IF EOD-DATE2 < EOD-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-DATE.

      * DATE RANGE MUST CORRESPOND WITH MONTHLY TRFILE
           IF (EOD-YR2 NOT = EOD-YR) OR
                (EOD-MO2 NOT = EOD-MO  )
              MOVE "DATE RANGE MUST BE WITHIN SAME MONTH AND YEAR"
                                                  TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-DATE.

       E-GL-BR1.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040GLBR1." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-GL-BRS.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY
              IF EXT-ROUTE-BUF = "00"
                 GO TO ENTER-DATE2
              ELSE
                 GO TO BR-02.
           IF NOT ENTER-KEY GO TO E-GL-BR1.
           MOVE VDUNUM0 TO GLBR1.
       E-GL-BR2.
           MOVE "ENNN040GLBR2." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-GL-BRS.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-GL-BR1.
           IF NOT ENTER-KEY GO TO E-GL-BR2.
           MOVE VDUNUM0 TO GLBR2.
           GO TO E-ACCT1.
       ALL-GL-BRS.
           MOVE "SN N040GLBR1." TO VDU.
           MOVE 0 TO GLBR1 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N040GLBR2." TO VDU.
           MOVE 9999 TO GLBR2 VDUNUM0.
           PERFORM SCREENIO.
       E-ACCT1.
           MOVE "ENNN070ACCT1." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-ACCTS.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-GL-BR1.
           IF NOT ENTER-KEY GO TO E-ACCT1.
           MOVE VDUNUM0 TO ACCOUNT1.
       E-ACCT2.
           MOVE "ENNN070ACCT2." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-ACCTS.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-ACCT1.
           IF NOT ENTER-KEY GO TO E-ACCT2.
           MOVE VDUNUM0 TO ACCOUNT2.
           GO TO E-TRANS-TYPE.
       ALL-ACCTS.
           MOVE "SN N070ACCT1." TO VDU.
           MOVE 0 TO ACCOUNT1 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N070ACCT2." TO VDU.
           MOVE 9999999 TO ACCOUNT2 VDUNUM0.
           PERFORM SCREENIO.
       E-TRANS-TYPE.
           MOVE "E  A010TRANSTYPE." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-TRANS-TYPES.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-ACCT2.
           IF NOT ENTER-KEY GO TO E-TRANS-TYPE.
           MOVE VDUBUF TO TRANS-TYPE.
           IF NOT (TRANS-TYPE = "P" OR "S" OR "L" OR "X" OR "C" OR "D"
                                    OR "B" OR "J" OR "K")
              MOVE "PLEASE ENTER P,S,L,X,C,D,B,J,K, OR F3 FOR ALL"
                 TO MESS
              PERFORM SEND-MESS
              GO TO E-TRANS-TYPE.
           GO TO E-LOAN1.
       ALL-TRANS-TYPES.
           MOVE "S  A030TRANSTYPE." TO VDU.
           MOVE "ALL" TO TRANS-TYPE VDUBUF.
           PERFORM SCREENIO.
       E-LOAN1.
           MOVE "ENNN060LOAN1." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-LOANS.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-TRANS-TYPE.
           IF NOT ENTER-KEY GO TO E-LOAN1.
           MOVE VDUNUM0 TO LOAN1.
       E-LOAN2.
           MOVE "ENNN060LOAN2." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-ACCTS.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-LOAN1.
           IF NOT ENTER-KEY GO TO E-LOAN2.
           MOVE VDUNUM0 TO LOAN2.
           GO TO E-CHECK1.
       ALL-LOANS.
           MOVE "SN N060LOAN1." TO VDU.
           MOVE 0 TO LOAN1 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N060LOAN2." TO VDU.
           MOVE 999999 TO LOAN2 VDUNUM0.
           PERFORM SCREENIO.
       E-CHECK1.
           MOVE "ENNN060CHECK1." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-CHECKS.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-LOAN2.
           IF NOT ENTER-KEY GO TO E-CHECK1.
           MOVE VDUNUM0 TO CHECK1.
       E-CHECK2.
           MOVE "ENNN060CHECK2." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-ACCTS.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-ACCT1.
           IF NOT ENTER-KEY GO TO E-CHECK2.
           MOVE VDUNUM0 TO CHECK2.
           GO TO E-ACTION1.
       ALL-CHECKS.
           MOVE "SN N060CHECK1." TO VDU.
           MOVE 0 TO CHECK1 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N060CHECK2." TO VDU.
           MOVE 999999 TO CHECK2 VDUNUM0.
           PERFORM SCREENIO.
       E-ACTION1.
           MOVE "E  A040ACTION1." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-ACTIONS.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-CHECK2.
           IF NOT ENTER-KEY GO TO E-ACTION1.
           MOVE VDUBUF TO ACTION1.
       E-ACTION2.
           MOVE "E  A040ACTION2." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-ACTION1.
           IF NOT ENTER-KEY GO TO E-ACTION2.
           MOVE VDUBUF TO ACTION2.
           GO TO E-INCL-DEPOSIT.
       ALL-ACTIONS.
           MOVE "S  A040ACTION1." TO VDU.
           MOVE SPACES TO ACTION1 VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A040ACTION2." TO VDU.
           MOVE "ZZZZ" TO ACTION2 VDUBUF.
           PERFORM SCREENIO.
       E-INCL-DEPOSIT.
           MOVE "E  A010INCLDEP." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-ACTION2.
           IF NOT ENTER-KEY GO TO E-INCL-DEPOSIT.
           MOVE VDUBUF TO INCL-DEPOSIT-FG.
           IF INCL-DEPOSIT-FG NOT = "Y"
              IF INCL-DEPOSIT-FG NOT = "N"
              MOVE "PLEASE ENTER Y OR N, PRESS F8 FOR HELP"
                 TO MESS
              PERFORM SEND-MESS
              GO TO E-INCL-DEPOSIT.

       MAKE-OUTFILE.

           IF EXT-ROUTE-BUF = "40" OR "50"
              MOVE "N" TO FIRST-TIME-ENTRY.

           IF FIRST-TIME-ENTRY = "Y"
              MOVE "N" TO FIRST-TIME-ENTRY
              GO TO END-PARM.

           MOVE "E  A010OUTFILE." TO VDU
           PERFORM SCREENIO.

           IF F7-KEY GO TO EXIT-PARM.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-INCL-DEPOSIT.

           MOVE VDUBUF TO OUTPUT-FILE.

           IF OUTPUT-FILE NOT = "Y" AND NOT = "N"
              GO TO MAKE-OUTFILE.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO END-PARM.

           IF OUTPUT-FILE = "N"
              MOVE "S  A000WKPATH." TO VDU
              MOVE SPACES TO  VDUBUF
              PERFORM SCREENIO
              GO TO END-PARM.

           MOVE EXT-FILPATH-BASE    TO DGL-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO DGL-PATH-MACHINE.

           IF OUTPUT-FILE = "Y"
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-MMDDYY
              MOVE DATE-MMDDYY TO DGL-PATH-DATE
              MOVE "S  A000WKPATH." TO VDU
              MOVE DGL-PATH  TO VDUBUF
              PERFORM SCREENIO.

      * VERIFY THAT IT DOES NOT EXIST
           MOVE DGL-PATH            TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              GO TO END-PARM.

       OVERWRITE-QUESTION.
           MOVE
           "GL EXTRACTION FILE EXISTS FOR THIS DATE. CONTINUE? (Y/N)"
              TO MESS
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "E  A000ANS." TO VDU.
           PERFORM SCREENIO.
           IF NOT ENTER-KEY
              GO TO OVERWRITE-QUESTION.
           IF VDUBUF = "Y"
              GO TO END-PARM.
           IF VDUBUF NOT = "N"
              GO TO OVERWRITE-QUESTION.
           IF VDUBUF = "N"
              GO TO BR-01.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.
       EXIT-PARM.
           EXIT.


      ********************************************
       DISPLAY-PARAMETERS SECTION.
      ********************************************

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

           MOVE "SN N040GLBR1." TO VDU.
           MOVE GLBR1    TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040GLBR2." TO VDU.
           MOVE GLBR2    TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N070ACCT1." TO VDU.
           MOVE ACCOUNT1 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N070ACCT2." TO VDU.
           MOVE ACCOUNT2 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010TRANSTYPE." TO VDU.
           MOVE TRANS-TYPE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN N060LOAN1." TO VDU.
           MOVE LOAN1 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N060LOAN2." TO VDU.
           MOVE LOAN2 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N060CHECK1." TO VDU.
           MOVE CHECK1 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N060CHECK2." TO VDU.
           MOVE CHECK2 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A040ACTION1." TO VDU.
           MOVE ACTION1 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040ACTION2." TO VDU.
           MOVE ACTION2 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010INCLDEP." TO VDU.
           MOVE INCL-DEPOSIT-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010OUTFILE." TO VDU.
           MOVE OUTPUT-FILE TO VDUBUF.
           PERFORM SCREENIO.

      ****************************************
       PROCESS-TRP-FILE SECTION.
      ****************************************
           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE EOD-DATE            TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE EOD-DATE2           TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE 0                   TO TP-CLASS TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-CLASS
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE.
           MOVE ALL "9"             TO QTRP1-WEND-CLASS
                                       QTRP1-WEND-ACCTNO
                                       QTRP1-WEND-LINE.

           MOVE TP-DATE              TO NDTE-DATE.
           MOVE 1                    TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE.

           IF ( TP-CLASS = 999 )
              MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
           ELSE
              COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1.

           IF ( TP-ACCTNO = 99999999 )
              MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
           ELSE
              COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1.

           IF ( TP-LINE = 999 )
              MOVE TP-LINE           TO QTRP1-WSBEG-LINE
           ELSE
              COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

           PERFORM START-TRP1-FILE.
       PROCESS-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO EXIT-PROCESS-TRP-FILE.

           IF TP-DATE > EOD-DATE2
              GO TO EXIT-PROCESS-TRP-FILE.

           IF (TP-ACCTNO < LOAN1 OR TP-ACCTNO > LOAN2)
              GO TO PROCESS-TRP-NEXT.

           IF TP-ACTIONCD2 < ACTION1 OR TP-ACTIONCD2 > ACTION2
              GO TO PROCESS-TRP-NEXT.

           PERFORM TRP1-DETAIL.

           GO TO PROCESS-TRP-NEXT.
       EXIT-PROCESS-TRP-FILE.
           PERFORM CLOSE-TRP1-FILE.

      *****************************************
       PROCESS-TRS-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRS1-FILE.

           MOVE BR-NO               TO TS-BRNO
                                       QTRS1-WBEG-BRNO
                                       QTRS1-WEND-BRNO.
           MOVE EOD-DATE            TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE.
           MOVE EOD-DATE2           TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WEND-DATE.

           MOVE 0                   TO TS-CLASS TS-ACCTNO
                                       TS-LAWCODE TS-LINE
                                       QTRS1-WBEG-CLASS
                                       QTRS1-WBEG-ACCTNO
                                       QTRS1-WBEG-LAWCODE
                                       QTRS1-WBEG-LINE.
           MOVE ALL "9"             TO QTRS1-WEND-CLASS
                                       QTRS1-WEND-ACCTNO
                                       QTRS1-WEND-LAWCODE
                                       QTRS1-WEND-LINE.
           PERFORM START-TRS1-FILE.

       PROCESS-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO EXIT-PROCESS-TRS-FILE.

           IF TS-DATE > EOD-DATE2
              GO TO EXIT-PROCESS-TRS-FILE.

           IF (TS-ACCTNO < LOAN1 OR TS-ACCTNO > LOAN2)
              GO TO PROCESS-TRS-NEXT.

           IF TS-ACTIONCD2 < ACTION1 OR TS-ACTIONCD2 > ACTION2
              GO TO PROCESS-TRS-NEXT.

           PERFORM TRS1-DETAIL.

           GO TO PROCESS-TRS-NEXT.

       EXIT-PROCESS-TRS-FILE.
           PERFORM CLOSE-TRS1-FILE.

      ****************************************
       PROCESS-TRL-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRL1-FILE.

           MOVE BR-NO               TO TL-BRNO
                                       QTRL1-WBEG-BRNO
                                       QTRL1-WEND-BRNO.
           MOVE EOD-DATE            TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE EOD-DATE2           TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.

           MOVE 0                   TO TL-CLASS TL-ACCTNO
                                       TL-LAWCODE TL-LINE
                                       QTRL1-WBEG-CLASS
                                       QTRL1-WBEG-ACCTNO
                                       QTRL1-WBEG-LAWCODE
                                       QTRL1-WBEG-LINE.
           MOVE ALL "9"             TO QTRL1-WEND-CLASS
                                       QTRL1-WEND-ACCTNO
                                       QTRL1-WEND-LAWCODE
                                       QTRL1-WEND-LINE.
           PERFORM START-TRL1-FILE.

       PROCESS-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO EXIT-PROCESS-TRL-FILE.

           IF TL-DATE > EOD-DATE2
              GO TO EXIT-PROCESS-TRL-FILE.

           IF (TL-ACCTNO < LOAN1 OR TL-ACCTNO > LOAN2)
              GO TO PROCESS-TRL-NEXT.

           IF TL-ACTIONCD2 < ACTION1 OR TL-ACTIONCD2 > ACTION2
              GO TO PROCESS-TRL-NEXT.

           PERFORM TRL1-DETAIL.

           GO TO PROCESS-TRL-NEXT.
       EXIT-PROCESS-TRL-FILE.
           PERFORM CLOSE-TRL1-FILE.

      ****************************************
       PROCESS-TRX-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRX1-FILE.

           MOVE BR-NO               TO TX-BRNO
                                       QTRX1-WBEG-BRNO
                                       QTRX1-WEND-BRNO.
           MOVE EOD-DATE            TO TX-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRX1-WBEG-DATE.
           MOVE EOD-DATE2           TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRX1-WEND-DATE.

           MOVE 0                   TO TX-CLASS TX-ACCTNO
                                       TX-LAWCODE TX-LINE
                                       QTRX1-WBEG-CLASS
                                       QTRX1-WBEG-ACCTNO
                                       QTRX1-WBEG-LAWCODE
                                       QTRX1-WBEG-LINE.
           MOVE ALL "9"             TO QTRX1-WEND-CLASS
                                       QTRX1-WEND-ACCTNO
                                       QTRX1-WEND-LAWCODE
                                       QTRX1-WEND-LINE.
           PERFORM START-TRX1-FILE.

       PROCESS-TRX-NEXT.
           PERFORM READ-TRX1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRX-PATH-OWNBR NOT = TX-BRNO)
              GO TO EXIT-PROCESS-TRX-FILE.

           IF TX-DATE > EOD-DATE2
              GO TO EXIT-PROCESS-TRX-FILE.

           IF (TX-ACCTNO < LOAN1 OR TX-ACCTNO > LOAN2)
              GO TO PROCESS-TRX-NEXT.

           PERFORM TRX1-DETAIL.

           GO TO PROCESS-TRX-NEXT.
       EXIT-PROCESS-TRX-FILE.
           PERFORM CLOSE-TRX1-FILE.

      ****************************************
       PROCESS-TRB-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRB1-FILE.

           MOVE BR-NO               TO TB-BRNO
                                       QTRB1-WBEG-BRNO
                                       QTRB1-WEND-BRNO.
           MOVE EOD-DATE            TO TB-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WBEG-DATE.
           MOVE EOD-DATE2           TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WEND-DATE.

           MOVE 0                   TO TB-CLASS TB-ACCTNO
                                       TB-LAWCODE TB-LINE
                                       QTRB1-WBEG-CLASS
                                       QTRB1-WBEG-ACCTNO
                                       QTRB1-WBEG-LAWCODE
                                       QTRB1-WBEG-LINE.
           MOVE ALL "9"             TO QTRB1-WEND-CLASS
                                       QTRB1-WEND-ACCTNO
                                       QTRB1-WEND-LAWCODE
                                       QTRB1-WEND-LINE.

           PERFORM START-TRB1-FILE.

       PROCESS-TRB-NEXT.
           PERFORM READ-TRB1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRB-PATH-OWNBR NOT = TB-BRNO)
              GO TO EXIT-PROCESS-TRB-FILE.

           IF TB-DATE > EOD-DATE2
              GO TO EXIT-PROCESS-TRB-FILE.

           IF (TB-ACCTNO < LOAN1 OR TB-ACCTNO > LOAN2)
              GO TO PROCESS-TRB-NEXT.

           IF TB-ACTIONCD2 < ACTION1 OR TB-ACTIONCD2 > ACTION2
              GO TO PROCESS-TRB-NEXT.

           PERFORM TRB1-DETAIL.

           GO TO PROCESS-TRB-NEXT.
       EXIT-PROCESS-TRB-FILE.
           PERFORM CLOSE-TRB1-FILE.

      ****************************************
       PROCESS-TRJ-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRJ1-FILE.

           MOVE BR-NO               TO TJ-BRNO
                                       QTRJ1-WBEG-BRNO
                                       QTRJ1-WEND-BRNO.
           MOVE EOD-DATE            TO TJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRJ1-WBEG-DATE.
           MOVE EOD-DATE2           TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRJ1-WEND-DATE.

           MOVE 0                   TO TJ-CLASS TJ-ACCTNO
                                       TJ-LAWCODE TJ-LINE
                                       QTRJ1-WBEG-CLASS
                                       QTRJ1-WBEG-ACCTNO
                                       QTRJ1-WBEG-LAWCODE
                                       QTRJ1-WBEG-LINE.
           MOVE ALL "9"             TO QTRJ1-WEND-CLASS
                                       QTRJ1-WEND-ACCTNO
                                       QTRJ1-WEND-LAWCODE
                                       QTRJ1-WEND-LINE.

           PERFORM START-TRJ1-FILE.

       PROCESS-TRJ-NEXT.
           PERFORM READ-TRJ1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRJ-PATH-OWNBR NOT = TJ-BRNO)
              GO TO EXIT-PROCESS-TRJ-FILE.

           IF TJ-DATE > EOD-DATE2
              GO TO EXIT-PROCESS-TRJ-FILE.

           IF (TJ-ACCTNO < LOAN1 OR TJ-ACCTNO > LOAN2)
              GO TO PROCESS-TRJ-NEXT.

           PERFORM TRJ1-DETAIL.

           GO TO PROCESS-TRJ-NEXT.
       EXIT-PROCESS-TRJ-FILE.
           PERFORM CLOSE-TRJ1-FILE.

      ****************************************
       PROCESS-TRK-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRK1-FILE.

           MOVE BR-NO               TO TK-BRNO
                                       QTRK1-WBEG-BRNO
                                       QTRK1-WEND-BRNO.
           MOVE EOD-DATE            TO TK-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRK1-WBEG-DATE.
           MOVE EOD-DATE2           TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRK1-WEND-DATE.

           MOVE 0                   TO TK-CLASS TK-ACCTNO
                                       TK-LAWCODE TK-LINE
                                       QTRK1-WBEG-CLASS
                                       QTRK1-WBEG-ACCTNO
                                       QTRK1-WBEG-LAWCODE
                                       QTRK1-WBEG-LINE.
           MOVE ALL "9"             TO QTRK1-WEND-CLASS
                                       QTRK1-WEND-ACCTNO
                                       QTRK1-WEND-LAWCODE
                                       QTRK1-WEND-LINE.

           PERFORM START-TRK1-FILE.

       PROCESS-TRK-NEXT.
           PERFORM READ-TRK1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRK-PATH-OWNBR NOT = TK-BRNO)
              GO TO EXIT-PROCESS-TRK-FILE.

           IF TK-DATE > EOD-DATE2
              GO TO EXIT-PROCESS-TRK-FILE.

           IF (TK-ACCTNO < LOAN1 OR TK-ACCTNO > LOAN2)
              GO TO PROCESS-TRK-NEXT.

           PERFORM TRK1-DETAIL.

           GO TO PROCESS-TRK-NEXT.
       EXIT-PROCESS-TRK-FILE.
           PERFORM CLOSE-TRK1-FILE.

      *****************************************
       PROCESS-TRC-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRC1-FILE.

           MOVE BR-NO      TO TC-BRNO
                              QTRC1-WBEG-BRNO
                              QTRC1-WEND-BRNO.
           MOVE EOD-DATE   TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE.
           MOVE EOD-DATE2  TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WEND-KEY-DATE.

           MOVE CHECK1              TO TC-CKNO 
                                       QTRC1-WBEG-CKNO.
           MOVE CHECK2              TO QTRC1-WEND-CKNO.

           MOVE 0                   TO TC-CLASS TC-SEQ
                                       QTRC1-WBEG-CLASS
                                       QTRC1-WBEG-SEQ.
           MOVE ALL "9"             TO QTRC1-WEND-CLASS
                                       QTRC1-WEND-SEQ.

           PERFORM START-TRC1-FILE.

       PROCESS-TRC-NEXT.
           PERFORM READ-TRC1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              GO TO EXIT-PROCESS-TRC-FILE.

           IF TC-KEY-DATE > EOD-DATE2
              GO TO EXIT-PROCESS-TRC-FILE.

      * IF IT'S A "C", CASH IS CKNO OF 0
           IF TC-CKNO < CHECK1 OR TC-CKNO > CHECK2
              GO TO PROCESS-TRC-NEXT.

           PERFORM TRC1-DETAIL.

           GO TO PROCESS-TRC-NEXT.
       EXIT-PROCESS-TRC-FILE.
           EXIT.

      *****************************************
       PROCESS-TRD-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRD1-FILE.

           MOVE BR-NO       TO TD-BRNO
                               QTRD1-WBEG-BRNO
                               QTRD1-WEND-BRNO.

           MOVE EOD-DATE    TO TD-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WBEG-DATE.
           MOVE EOD-DATE2   TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WEND-DATE.

           PERFORM START-TRD1-FILE.

       PROCESS-TRD-NEXT.
           PERFORM READ-TRD1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRD-PATH-OWNBR NOT = TD-BRNO)
              GO TO EXIT-PROCESS-TRD-FILE.

           IF TD-DATE > EOD-DATE2
              GO TO EXIT-PROCESS-TRD-FILE.

           PERFORM TRD1-DETAIL.

           GO TO PROCESS-TRD-NEXT.
       EXIT-PROCESS-TRD-FILE.
           PERFORM CLOSE-TRD1-FILE.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 3 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP       :" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH         :" TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH         :" TO RANGE-LINE
              MOVE END-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE.
           MOVE "BEGINNING  DATE    :" TO RANGE-LINE.
           MOVE EOD-DATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING  DATE       :" TO RANGE-LINE.
           MOVE EOD-DATE2 TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG GL BRANCH      :" TO RANGE-LINE.
           MOVE GLBR1 TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END GL BRANCH      :" TO RANGE-LINE.
           MOVE GLBR2 TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG GL ACCOUNT     :" TO RANGE-LINE.
           MOVE ACCOUNT1 TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END GL ACCOUNT     :" TO RANGE-LINE.
           MOVE ACCOUNT2 TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TRANSACTION TYPE   :" TO RANGE-LINE.
           MOVE TRANS-TYPE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG LOAN ACCOUNT   :" TO RANGE-LINE.
           MOVE LOAN1 TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END LOAN ACCOUNT   :" TO RANGE-LINE.
           MOVE LOAN2 TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG CHECK NUMBER   :" TO RANGE-LINE.
           MOVE CHECK1 TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END CHECK NUMBER   :" TO RANGE-LINE.
           MOVE CHECK2 TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG ACTIONCD2      :" TO RANGE-LINE.
           MOVE ACTION1 TO R-ALPHA.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END ACTIONCD2      :" TO RANGE-LINE.
           MOVE ACTION2 TO R-ALPHA.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE DEPOSITS   :" TO RANGE-LINE.
           MOVE INCL-DEPOSIT-FG TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CREATE OUTPUT FILE :" TO RANGE-LINE
           MOVE OUTPUT-FILE TO R-ALPHA.
           PERFORM WRITE-DETAIL-LINE.

           IF OUTPUT-FILE = "Y"
              MOVE "RECORDS WRITTEN    :" TO RANGE-LINE
              MOVE RECORDS-WRITTEN TO R-NUM
              PERFORM WRITE-DETAIL-LINE

              MOVE "TOTAL DEBITS       :" TO RANGE-LINE
              MOVE TOTAL-DEBITS TO R-DECIMAL
              PERFORM WRITE-DETAIL-LINE

              MOVE "TOTAL CREDITS      :" TO RANGE-LINE
              MOVE TOTAL-CREDITS TO R-DECIMAL
              PERFORM WRITE-DETAIL-LINE.

       END-RANGE.
           EXIT.


      ****************************
       PRINT-GL-DETAIL SECTION.
      ****************************
           IF ED-CODE-ACCT = SAVE-ED-CODE-ACCT
              IF ED-TR-SUBKEY = SAVE-ED-TR-SUBKEY
                 MOVE 1 TO LN-FEED
                 GO TO SKIP-PRINT-TR-INFO.
           MOVE ED-CODE-ACCT TO SAVE-ED-CODE-ACCT.
           MOVE ED-TR-SUBKEY TO SAVE-ED-TR-SUBKEY.
           IF ED-TR-CODE = "P"
              MOVE "PAYMENT:" TO D-TR-DESC
              MOVE ED-TR-TYPE TO D-TR-CODE2
           ELSE IF ED-TR-CODE = "S"
              MOVE "SALES FIN" TO D-TR-DESC
           ELSE IF ED-TR-CODE = "L"
              MOVE "DIR LOAN" TO D-TR-DESC
           ELSE IF ED-TR-CODE = "X"
              MOVE "TRANSFER" TO D-TR-DESC
           ELSE IF ED-TR-CODE = "C"
              MOVE "CHECK" TO D-TR-DESC
           ELSE IF ED-TR-CODE = "D"
              MOVE "DEPOSIT" TO D-TR-DESC
           ELSE IF ED-TR-CODE = "B"
              MOVE "BRANCH XFER" TO D-TR-DESC
           ELSE IF ED-TR-CODE = "C"
              MOVE "CLASS XFER" TO D-TR-DESC
           ELSE IF ED-TR-CODE = "K"
              MOVE "CONVERSION" TO D-TR-DESC
           ELSE
              MOVE ED-TR-CODE TO D-TR-DESC.
           MOVE ED-TR-ACCT TO D-TR-ACCT.
           MOVE 2 TO LN-FEED.

       SKIP-PRINT-TR-INFO.
           MOVE ED-NO      TO GL-BR-ACCOUNT.
           MOVE GL-BRANCH  TO D-G-BRNO.
           MOVE "-"        TO D-G-HYPHEN.
           MOVE GL-ACCOUNT TO D-G-GLNO.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           IF IO-FG = 9
              PERFORM CREATE-LOG
              MOVE "* *  N O T   F O U N D  * *" TO GL-DESCRIPTION.
           PERFORM CLOSE-GL1-FILE.
           MOVE GL-DESCRIPTION TO D-G-DESCRIPTION.
           MOVE ED-TP-REFNO                TO D-TR-REFNO. 
           MOVE ED-TP-PLTYPE               TO D-TR-PLTYPE.
           MOVE ED-UPDATED TO D-G-POST.

       PRINT-GL-TRANS.
           IF ED-AMT(2) = 0
              GO TO PRINT-GL-SFREG.
           MOVE "TRANSACTION JOURNAL" TO D-SOURCE-DESC.
           IF ED-AMT(2) < 0
              MOVE ED-AMT(2) TO D-G-CR
              ADD ED-AMT(2) TO TOT-CR GRAND-TOT-CR
           ELSE
              MOVE ED-AMT(2) TO D-G-DR
              ADD ED-AMT(2) TO TOT-DR GRAND-TOT-DR.
           PERFORM WRITE-DETAIL-LINE.
       PRINT-GL-SFREG.
           IF ED-AMT(3) = 0
              GO TO PRINT-GL-DLREG.
           MOVE "SALES FINANCE REGISTER" TO D-SOURCE-DESC.
           IF ED-AMT(3) < 0
              MOVE ED-AMT(3) TO D-G-CR
              ADD ED-AMT(3) TO TOT-CR GRAND-TOT-CR
           ELSE
              MOVE ED-AMT(3) TO D-G-DR
              ADD ED-AMT(3) TO TOT-DR GRAND-TOT-DR.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-DLREG.
           IF ED-AMT(4) = 0
              GO TO PRINT-GL-TFJRNL.
           MOVE "DIRECT LOAN REGISTER" TO D-SOURCE-DESC.
           IF ED-AMT(4) < 0
              MOVE ED-AMT(4) TO D-G-CR
              ADD ED-AMT(4) TO TOT-CR GRAND-TOT-CR
           ELSE
              MOVE ED-AMT(4) TO D-G-DR
              ADD ED-AMT(4) TO TOT-DR GRAND-TOT-DR.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-TFJRNL.
           IF ED-AMT(5) = 0
              GO TO PRINT-GL-CHKS.
           MOVE "SERV/PUR TRANSFER JOURNAL" TO D-SOURCE-DESC.
           IF ED-AMT(5) < 0
              MOVE ED-AMT(5) TO D-G-CR
              ADD ED-AMT(5) TO TOT-CR GRAND-TOT-CR
           ELSE
              MOVE ED-AMT(5) TO D-G-DR
              ADD ED-AMT(5) TO TOT-DR GRAND-TOT-DR.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-CHKS.
           IF ED-AMT(6) = 0
              GO TO PRINT-GL-DEP.
           MOVE "CHECKS" TO D-SOURCE-DESC.
           IF ED-AMT(6) < 0
              MOVE ED-AMT(6) TO D-G-CR
              ADD ED-AMT(6) TO TOT-CR GRAND-TOT-CR
           ELSE
              MOVE ED-AMT(6) TO D-G-DR
              ADD ED-AMT(6) TO TOT-DR GRAND-TOT-DR.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-DEP.
           IF ED-AMT(7) = 0
              GO TO PRINT-GL-BRANCH-TRANSFER.
           MOVE "DEPOSIT" TO D-SOURCE-DESC.
           IF ED-AMT(7) < 0
              MOVE ED-AMT(7) TO D-G-CR
              ADD ED-AMT(7) TO TOT-CR GRAND-TOT-CR
           ELSE
              MOVE ED-AMT(7) TO D-G-DR
              ADD ED-AMT(7) TO TOT-DR GRAND-TOT-DR.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-BRANCH-TRANSFER.
           IF ED-AMT(8) = 0
              GO TO PRINT-GL-CLASS-TRANSFER.
           MOVE "BRANCH TRANSFER JOURNAL" TO D-SOURCE-DESC.
           IF ED-AMT(8) < 0
              MOVE ED-AMT(8) TO D-G-CR
              ADD ED-AMT(8) TO TOT-CR GRAND-TOT-CR
           ELSE
              MOVE ED-AMT(8) TO D-G-DR
              ADD ED-AMT(8) TO TOT-DR GRAND-TOT-DR.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-CLASS-TRANSFER.
           IF ED-AMT(9) = 0
              GO TO PRINT-GL-ACCT-CONV.
           MOVE "CLASS TRANSFER JOURNAL" TO D-SOURCE-DESC.
           IF ED-AMT(9) < 0
              MOVE ED-AMT(9) TO D-G-CR
              ADD ED-AMT(9) TO TOT-CR GRAND-TOT-CR
           ELSE
              MOVE ED-AMT(9) TO D-G-DR
              ADD ED-AMT(9) TO TOT-DR GRAND-TOT-DR.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-ACCT-CONV.
           IF ED-AMT(10) = 0
              GO TO EXIT-PRINT-GL-DETAIL.
           MOVE "ACCOUNT CONVERSION JOURNAL" TO D-SOURCE-DESC.
           IF ED-AMT(10) < 0
              MOVE ED-AMT(10) TO D-G-CR
              ADD ED-AMT(10) TO TOT-CR GRAND-TOT-CR
           ELSE
              MOVE ED-AMT(10) TO D-G-DR
              ADD ED-AMT(10) TO TOT-DR GRAND-TOT-DR.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-GL-DETAIL.
           EXIT.

      ****************************
       PRINT-GL-TOTALS SECTION.
      ****************************

           MOVE 2 TO LN-FEED.

           PERFORM WRITE-DETAIL-LINE.

           MOVE "RECAP TOTALS :" TO DETAIL-LINE.
           MOVE TOT-DR TO D-G-DR.
           MOVE TOT-CR TO D-G-CR.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 0 TO TOT-DR TOT-CR.

       PRINT-GL-TOTS-EXIT.
           EXIT.

      ****************************
       PRINT-GRAND-TOTALS SECTION.
      ****************************
           IF OUTPUT-FILE = "Y"
              GO TO PRINT-GRAND-TOTS-EXIT.

           MOVE 2 TO LN-FEED.

           PERFORM WRITE-DETAIL-LINE.

           MOVE "GRAND TOTALS :" TO DETAIL-LINE.
           MOVE GRAND-TOT-DR TO D-G-DR.
           MOVE GRAND-TOT-CR TO D-G-CR.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GRAND-TOTS-EXIT.
           EXIT.

      ***********************************
       UPDATE-WORK-FILE SECTION.
      ***********************************

           IF ( GLAMT = 0 ) OR
              ( GLNO-BRANCH < GLBR1 OR GLNO-BRANCH > GLBR2 ) OR
              ( GLNO-ACCT < ACCOUNT1 OR GLNO-ACCT > ACCOUNT2 )
              MOVE 0 TO GLAMT GLNO
              GO TO UPDATE-WORK-FILE-EXIT.

           IF OUTPUT-FILE = "Y"
              GO TO UPDATE-WORK-FILE-END.

           MOVE 1 TO SAVE-ED-TR-SEQNO.
       FIND-UNIQUE-ED-KEY.
           MOVE SPACES TO ED-KEY.

           MOVE TR-CODE          TO ED-TR-CODE.
           MOVE TR-ACCT          TO ED-TR-ACCT.
           MOVE TR-SUBKEY        TO ED-TR-SUBKEY.
           MOVE SAVE-ED-TR-SEQNO TO ED-TR-SEQNO.
           MOVE GLNO             TO ED-NO.

           PERFORM READ-ED1-FILE.
           IF IO-FG = 0
              IF SAVE-ED-TR-SEQNO < 999
                 ADD 1 TO SAVE-ED-TR-SEQNO
                 GO TO FIND-UNIQUE-ED-KEY
              ELSE
                 GO TO SKIP-CREATE-WK.

           IF IO-FG = 9
              GO TO CREATE-WK.

       SKIP-CREATE-WK.
           IF TR-CODE = "P"
              ADD GLAMT TO ED-AMT(2)
           ELSE
           IF TR-CODE = "S"
              ADD GLAMT TO ED-AMT(3)
           ELSE
              IF TR-CODE = "L"
                 ADD GLAMT TO ED-AMT(4)
              ELSE
              IF TR-CODE = "X"
                 ADD GLAMT TO ED-AMT(5)
              ELSE
              IF TR-CODE = "C"
                 PERFORM TR1-CODE-C          
              ELSE
              IF TR-CODE = "D"
                 ADD GLAMT TO ED-AMT(7)
              ELSE
              IF TR-CODE = "B"
                 ADD GLAMT TO ED-AMT(8)
              ELSE
              IF TR-CODE = "J"
                 ADD GLAMT TO ED-AMT(9)
              ELSE
              IF TR-CODE = "K"
                 ADD GLAMT TO ED-AMT(10).

           PERFORM REWRITE-ED1-FILE.

           GO TO UPDATE-WORK-FILE-END.

       CREATE-WK.
           MOVE SPACES TO ED-REC.

           MOVE TR-CODE          TO ED-TR-CODE.
           MOVE TR-ACCT          TO ED-TR-ACCT.
           MOVE TR-SUBKEY        TO ED-TR-SUBKEY.
           MOVE SAVE-ED-TR-SEQNO TO ED-TR-SEQNO.
           MOVE GLNO             TO ED-NO.

           IF TR-CODE = "P"
              MOVE TR-TYPE-TRCD TO ED-TR-TYPE
              MOVE TR-REFNO     TO ED-TP-REFNO
              MOVE TR-PLTYPE    TO ED-TP-PLTYPE.

           MOVE 0 TO ED-AMT(2) ED-AMT(3) ED-AMT(4)
                     ED-AMT(5) ED-AMT(6) ED-AMT(7)
                     ED-AMT(8) ED-AMT(9) ED-AMT(10).

           MOVE "G" TO ED-ID.

           IF TR-CODE = "P"
              ADD GLAMT TO ED-AMT(2)
           ELSE
           IF TR-CODE = "S"
              ADD GLAMT TO ED-AMT(3)
           ELSE
              IF TR-CODE = "L"
                 ADD GLAMT TO ED-AMT(4)
              ELSE
              IF TR-CODE = "X"
                 ADD GLAMT TO ED-AMT(5)
              ELSE
              IF TR-CODE = "C"
                 PERFORM TR1-CODE-C      
              ELSE
              IF TR-CODE = "D"
                 ADD GLAMT TO ED-AMT(7)
              ELSE
              IF TR-CODE = "B"
                 ADD GLAMT TO ED-AMT(8)
              ELSE
              IF TR-CODE = "J"
                 ADD GLAMT TO ED-AMT(9)
              ELSE
              IF TR-CODE = "K"
                 ADD GLAMT TO ED-AMT(10).

           PERFORM WRITE-ED1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       UPDATE-WORK-FILE-END.

           IF OUTPUT-FILE = "Y"
              PERFORM CREATE-DGL-OUTPUT-FILE.

           MOVE 0 TO GLAMT GLNO.

       UPDATE-WORK-FILE-EXIT.
           EXIT.


      **************************************************
       CREATE-DGL-OUTPUT-FILE SECTION.
      **************************************************
           MOVE SPACES TO DGL-REC.

           MOVE 0 TO DGL-BRANCH
                     DGL-CHK-ACCTNO
                     DGL-ACCT-GL
                     DGL-AMOUNT
                     DGL-DATE-MMDDYY.

           MOVE BR-NO        TO DGL-BRANCH.
           MOVE TR-CODE      TO DGL-CODE.
           MOVE TR-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO DGL-DATE-MMDDYY.
           MOVE TR-ACCT      TO DGL-CHK-ACCTNO

           IF TR-CODE = "P"
              MOVE TR-TYPE-TRCD  TO DGL-TRCD
              MOVE TR-REFNO      TO DGL-REFNO
              MOVE TR-PLTYPE     TO DGL-PLTYPE
              MOVE TR-POSTING-BR TO DGL-POSTING-BR
              MOVE TR-OTHBR-FG   TO DGL-OTHBR-FG.

           IF TR-CODE = "B" OR "P" OR "L" OR "S"
              MOVE TR-ACTIONCD2  TO DGL-ACTIONCD2.

           MOVE GLNO  TO DGL-ACCT-GL.
           MOVE GLAMT TO DGL-AMOUNT.
           INSPECT DGL-AMOUNT REPLACING LEADING "+" BY " ".

           IF GLAMT < 0
              ADD GLAMT TO TOTAL-CREDITS
           ELSE
              ADD GLAMT TO TOTAL-DEBITS.

           MOVE X"0D" TO WK-CR.
           MOVE X"0A" TO WK-LF.
           ADD 1 TO DGL-KEY.
           PERFORM WRITE-DGL-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           ADD 1 TO RECORDS-WRITTEN.

       CREATE-DGL-OUTPUT-FILE-EXIT.
           EXIT.


      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE " " TO DETAIL-LINE.

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

           IF OUTPUT-FILE = "Y"
              MOVE SPACES TO HEAD3.
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 6 TO LN-COUNT.
           MOVE 2 TO LN-FEED.


       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO GLRCAP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GLRCAP-SYSDATE.
           DISPLAY GLRCAP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GLRCAP-FORM-FIELDS TO WR-BUF.
           MOVE GLRCAP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE GLRCAP-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLRCAP-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/GLRCAP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
      *  NOTE: CHECK-UPDATE-BR & TD-UPD-BR ARE DUMMY PARAGRAPHS
      *        BECAUSE COPY MEMBER LPEOD5TR PERFORMS THEM WHEN
      *        UPDATING.
       CHECK-UPDATE-BR.
           EXIT.
       TD-UPD-BR.
           EXIT.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       TR1-CODE-C.
           IF (TC-TYPE = "BK" OR "ID" OR "IW")
               ADD GLAMT TO ED-AMT(7)
           ELSE
               ADD GLAMT TO ED-AMT(6).

       GET-CODE.
           MOVE "GC"   TO CDB-TYPE.
      *--> SET BEFORE     CDB-BRANCH.
      *--> SET BEFORE     CDB-CODE.
           PERFORM READ-CDB1-FILE.
       TEST-BR-FEE-GL-BRNO.
           IF BR-FEE-GL-BRNO NUMERIC
              IF BR-FEE-GL-BRNO NOT = 0
                 MOVE BR-FEE-GL-BRNO TO GLNO-BR.
       PROGRESS-MSG.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CREATE-LOG.
      *    MOVE ER-PATH TO ACCESS-BUF.
      *    PERFORM ACCESS-CALL.
      *    IF STAT NOT = "00"
      *       OPEN OUTPUT ER-FILE
      *       CLOSE ER-FILE.
      *    MOVE E-FILE TO E-FILEX.
      *    MOVE FILE-STAT TO E-CODE.
      *    MOVE ERROR-MSG TO MESS.
      *    PERFORM OPEN-ER-FILE.
      *    MOVE MESS TO ER-REC.
      *    MOVE "PROGRAM =" TO ER-PROGRAM.
      *    MOVE "LPEOD5" TO ER-NAME.
      *    MOVE EXT-CONAME-SYSDATE TO ER-DATE.
      *    PERFORM WRITE-ER-REC.
      *    PERFORM CLOSE-ER-FILE.
       SAVE-ERROR-MSG.
           EXIT.
       CONVERT-TIME.
           IF H-AND-M NOT GREATER THAN 1159
              MOVE "AM" TO T-AMPM
           ELSE
              MOVE "PM" TO T-AMPM.
           IF H-AND-M IS NOT LESS THAN 1300
              SUBTRACT 1200 FROM H-AND-M.
           IF CUR-HH = 0
              MOVE 12 TO CUR-HH.
           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":" TO T-COL.
           MOVE " " TO T-SPACE.
      *----------------------------------------------------------------

       COPY "LIBLP/LPEOD5TR.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".

       COPY "LIBGB/ARRAYBR.CPY".
       COPY "LIBLP/ARRAYLC.CPY".
       COPY "LIBGL/ARRAYGI.CPY".

      ************************
       IO-ROUTINES SECTION.
      ************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRXGS_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPTRKGS_SQL.CPY".
       COPY "LIBLP/LPTRJGS_SQL.CPY".
       COPY "LIBLP/LPTRDGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPTRBGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPED1IN.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
       COPY "LIBLP/LPTRD1RN.CPY".
       COPY "LIBLP/LPTRL1RN.CPY".
       COPY "LIBLP/LPTRS1RN.CPY".
       COPY "LIBLP/LPTRB1RN.CPY".
       COPY "LIBLP/LPTRX1RN.CPY".
       COPY "LIBLP/LPTRJ1RN.CPY".
       COPY "LIBLP/LPTRC1RN.CPY".
       COPY "LIBLP/LPTRK1RN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".

       OPEN-DGL-FILE.
           PERFORM OPEN-IT.
           MOVE TEMP-PATH TO E-FILE.
           OPEN I-O DGL-FILE.
           IF IO-FG = 8
              GO TO OPEN-DGL-FILE.
           IF IO-FG = 7
              CLOSE DGL-FILE
              GO TO OPEN-DGL-FILE.
           UNLOCK DGL-FILE.
       READ-DGL-FILE.
           PERFORM READ-IT.
           MOVE TEMP-PATH TO E-FILE.
           MOVE DGL-KEY TO E-KEYX.
           READ DGL-FILE.
           IF IO-FG = 8
              GO TO READ-DGL-FILE.
       WRITE-DGL-FILE.
           PERFORM WRITE-IT.
           MOVE TEMP-PATH TO E-FILE.
           MOVE DGL-KEY TO E-KEYX.
           WRITE DGL-REC.
           IF IO-FG = 8
              GO TO WRITE-DGL-FILE.
           UNLOCK DGL-FILE.
       CLOSE-DGL-FILE.
           PERFORM CLOSE-IT.
           CLOSE DGL-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.


      *****************************************
       END-PROGRAM SECTION.
      *****************************************
       END-RUN.
           PERFORM PRINT-RANGE-LINES.

           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE ED-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
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
              DESTROY GLRCAP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
