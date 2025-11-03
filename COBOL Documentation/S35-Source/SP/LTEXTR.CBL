       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LTEXTR.
       DATE-WRITTEN.    012302.
      **************************************************************************
      *                      (SP)
      *    DESCRIPTION:  DATA WAREHOUSE LOAN TRAILER EXTRACTIONS
      *
      * NOTE: ALL DATES ARE YYYYMMDD
      *
      * CREATES OUTPUT FILE /USR/EXTRACT/REPO,       RSZ 1174 INCLUDING CR, LF
      *                     /USR/EXTRACT/ADDON, (LTI)RSZ  371 INCLUDING CR, LF
      *                     /USR/EXTRACT/AUTO_INS,   RSZ  442 INCLUDING CR, LF
      *                     /USR/EXTRACT/LIENHOLDER, RSZ  700 INCLUDING CR, LF
      *                     /USR/EXTRACT/BANKRUPT,   RSZ  562 INCLUDING CR, LF
      *                     /USR/EXTRACT/COLLATERAL, RSZ  532 INCLUDING CR, LF
      *                     /USR/EXTRACT/SCHEDULE,   RSZ  280 INCLUDING CR, LF
      *                     /USR/EXTRACT/DEFICIENCY  RSZ  648 INCLUDING CR, LF
      *                     /USR/EXTRACT/FOLLOWUP    RSZ  168 INCLUDING CR, LF
      *                     /USR/EXTRACT/RESCHEDULE  RSZ  192 INCLUDING CR, LF
      *                     /USR/EXTRACT/F9MISC      RSZ  762 INCLUDING CR, LF
      *                     /USR/EXTRACT/LEGAL,      RSZ  585 INCLUDING CR, LF
      *                     /USR/EXTRACT/JUDGMENT,   RSZ  450 INCLUDING CR, LF
      *                     /USR/EXTRACT/DEBITCARD,  RSZ  450 INCLUDING CR, LF
      * REV:
      *     012302 REGIONAL ACCEPTANCE, PR 1675
      * BLV 020214 FIXED NOT FILLING IN LT-ACCTNO & LT-CODE IN XLT RECORDS
      * BLV 020822 ADDED NEW LS FIELDS FROM NEW REPO PAGE 4, REGACC PR# 1840
      * BAH 020923 ADDED LT-B-BDDATE BNKRPT DISMISSAL REGACC PR# 1901
      * BAH 020924 ADDED LS-X-TITLE-SENT-DATE REGACC PR# 2
      * BLV 021004 FIXED COMP FIELDS BOO BOO, (FROM PR# 1840)
      * BLV 021111 FIXED LT-SEQNO NOT GETTING MOVED IN FOR LTI RECORDS
      * BLV 030327 ADD EXTRACT OF TRIAD FILE (LR), REGACC PR# 0026
      * BLV 030411 FIX XLR-CL-AMT-FEES, MISSING .99, REGACC PR#0026
      * BLV 030429 ADDED XLR-CL-NUM-RPC, XLR-CL-LAST-CYCLE-DATE
      * BAH 030530 ADDED EXTRACT OF LIENHOLDER INFO (LT) CODE = "L"
      *            REGACC PR# 20
      * BAH 031202 OPEN LRFILE I/O SO IT WILL CREATE IT EMPTY IF NOT FOUND
      *            CAUSING OPEN ERROR 35 AT CENTRIX.
      * BAH 040414 ADDED PAYOFF TITLE PROCESSING LT LIEN SEQ 5 AND 6,
      *            REGACC PR# 53
      *  CS 040712 ADDED XLR-CL-DIALER-COUNTER, XLR-CL-BILL-BAL [PR#68 REGACC]
      * BLV 040730 ADDED XLTP-RECORD, TRAILER PAYMENT SCHEDULE EXTRACTION,
      *            CENTRIX PR# 2356
      *  CS 040907 ADD REGION-NUMBER TO ALL OUTPUT FILES  [PR#76 REGACC]
      * BLF 050127 IF THERE IS NO SEQ 2 LTC RECORD, NUMERIC FIELDS FROM SEQ 1
      *            WERE GETTING WIPED OUT (MILEAGE, VALUE, SELLPR, MISC)!
      *  CS 050908 ADD DEFICIENCY (XLSD) REGACC [PR# 112]
      * BLF 050909 FIXED ATTORNEY NAME -IT WAS GETTING MOVED TO THE ADDRESS
      *            FIELD & WIPED OUT, LENDMARK
      * BLF 050927 FIXED TRUSTEE NAME & CLERK NAME - THEY WERE GETTING
      *            MOVED TO THE ADDRESS FIELDS & WIPED OUT, CENTRIX
      *
      * KEX 051116 {PR#00117} REGIONAL ACCEPTANCE <A15> [K.MULLEN]
      *            ADDED LOGIC TO SET FIELDS:
      *
      *               XLR-CL-BPTP-LT-COUNTER
      *               XLR-CL-BPTP-ST-COUNTER
      *               XLR-CL-BPTP-SCEN-ID
      * BAH 2007.0131 REGACC PR# 162 NEW DEF POLICY FOR 6 MONTHS PAID THRU
      *               ELAPSED BETWEEN DEFERMENTS, HAD TO ADD;
      *               LR-PX10-LAST-DEF-PDTH        PIC 9(6)    COMP-6.
      *               LR-PX10-PREV-DEF-PDTH        PIC 9(6)    COMP-6.
      *  CS 070302  ADDED LR-CL-LAST-AL-DATE, LR-CL-LAST-AL-ASSESSMENT
      *                                               PR#152-R REGIONAL
      * BAH 070313 TRITON PR# 2937 ADDED LS-F FOLLOW UP INFO
      * BAH 070504 ADDED NEW INSURANCE TRACKING FIELDS FROM THE LT-A RECORD
      *            REGACC PR# 173
      *  CS 070912 ADDED NEW XLSD FIELDS FROM LS-D (DEFICIENCY) RECORD USED
      *            FOR REINSTATEMENT STATUS REGACC PR#180
      *  CS 070925 REMOVE LS-D-SALES PRICE PER KEVIN  PR#180
      * BLF 071004 FIX XLR-CL-LAST-AL-DATE TO BE YYYYMMDD INSTEAD OF MMDDYY,
      *            ADJUST/RENAME FILLERS TO BE OK IN XFD, PER KEVIN, 152
      * BAH 080227 ADDED XLTL-TITLE-RCV-DATE, XLTL-TITLE-LIEN-DATE,
      *             XLTL-TITLE-NO, XLTL-LIEN-LETTER-SENT, XLTL-SEND-ASAP
      * BAH 080303 ADDED XLTB-WHO-FILED, XLTB-ENDORSER-FILED, XLTB-CLAIM-AMT
      * BAH 081016 ADDED XLTB-ACTIVE-INACTIVE-AMT, REGACC PR# 223
      * BAH 100820 ADDED XLTL-TITLE-5-ADR2, REGACC PR# 270
      * BAH 110328 ADDED RESCHEDULE/SCRA "LS-R", REGACC PR# 289/290
      * BAH 111205 ADDED COLLATERAL ONLY OPTION, REGACC PR# 302
      * BAH 120403 CHANGED XLS-D-SALES-DATE, XLS-D DATES TO 8 DIGITS
      * BAH 120918 ADDED XLTL-TITLE-4-TITLE-STATE AND XLTL-TITLE-6-RELEASE-TYPE
      *                                               REGACC PR# 320
      * BAH 120925 ADDED XLTL-LIEN-4-STATUS-CODE REGACC PR# 321
      * BAH 121001 ADDED XLTL-CARRIER-TYPE, XLTL-CARRIER-ACCTNO,REGACC PR# 322
      *  CS 121010 ADD XLR-CL-NONACCRUAL-STATUS, XLR-CL-NONACCRUAL-START-DATE
      *            XLR-CL-MONTHS-PAY-NAPP-COUNTER   REGACC PR#323
      * BAH 130122 XLS-D-DATE-CHECK-RECV, XLS-D-SALES-DATE,
      *            XLS-D-CALC-NXDUE-AFT-REPO AND XLS-D-CALC-NXTDUE-LC-AFT-REPO
      *            WERE MOVING DATE-MMDDYYYY INSTEAD OF DATE-YYYYMMDD PL# 782
      * BAH 130227 ADDED NEW LS-Y AND LS-D FIELDS FOR REGACC PR# 333
      *  CS 130520 ADD XLR-PX10-ENVELOPE-ONLY, XLR-PX10-ENVELOPE-MAKER-COMAKER
      *            LR-PX10-LAST-DEF-TRCD, LR-PX10-PREV-DEF-TRCD
      *            LR-PX10-LAST-DF-REFNO, LR-PX10-PREV-DF-REFNO, LR-PX10-TDR1
      *            LR-PX10-TDR1-DATE, LR-PX10-TDR2, LR-PX10-TDR2-DATE
      *                                                       REGACC PR#338
      *  CS 131209 ADD XLR-CL-NONACCRUAL-STOP-DATE REGACC PR#347
      * BAH 140513 ADDED WRITE-REPO-FLAG TO DETERMINE IF EITHER LS-X OR
      *            LS-Y RECORDS WERE FOUND TO CREATE A REPO RECORD, PL# 845
      *                                                    AUTOMOTIVE CREDIT
      * BAH 141121 ADDED XLSM F9 MISC FIELDS, SOAUTO PR# 4651
      * KEC 150121 {PR#00356} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *            ***NEW*** TRIAD 9
      *            WAS RSZ=1799; NOW RSZ=1880
      *            XLR-RO20-LN-CAT
      *            XLR-RO20-COLL-DATE-BILL-EQV
      *            XLR-RO20-DATE-FIRST-COLLS-DA
      *            XLR-RO20-COLL-BALANCE-INITIAL
      *            XLR-RO20-COLL-AMT-ARREARS
      *            XLR-RO20-COLL-BALANCE-ACTUAL
      *            XLR-RO20-COLL-PTP
      *            XLR-RO20-BAR-FACTOR
      * BAH 150512 EXPANDED ALL OUTPUT KEYS FROM 9(7) TO 9(9), WORLD #1046A
      * BAH 160201 SPLIT OUT LTFILE, PD0003
      * BAH 160209 SPLIT OUT LSFILE, PD0003
      * BAH 160502 ADDED BANKRUPT REAFFIRMATION/DISCHARGE TO THE
      *            BANKRUPT EXTRACT FILE, WORLD PR# 1150
      * BAH 160503 EXTRACT THE LEGAL LS TRAILER, SOAUTO PR#5064
      * BAH 160719 ADDED XLS-KEYS FOR SOAUTO PR#5133
      * BAH 170410 EXTRACT THE JUDGMENT LS TRAILER, SUPPORT PR#5277
      * BAH 170918 XLS-J-GARN-DATE WAS NOT IN CORRECT FORMAT, WORLD PR#1279
      * BAH 171016 ADDED XLTB-INT-LC-DUE, XLTB-OTH-DUE, XLTB-TDUEAMT,
      *                                                       WORLD PR#1282 
      * BAH 180306 ADDED XLT-D DEBIT CARD PROCEEDS, WORLD #1306
      *            DEBITCARD FILE
      * BAH 180514 MISSED FILLING IN XLTD-DEBIT-CARD-AMT, WORLD #1319
      *  CS 180508 CHANGED MOVE OF LT?-ACCTNO(8) TO A WORKING STORAGE
      *            WS-A15-NUMBER 9(6), WS-A15-SEQNO 9(2) TO MATCH A15 OUTPUT
      *  CS 180511 CHANGED MOVE OF LS?-ACCTNO(8) TO A WORKING STORAGE
      *            WS-A15-NUMBER 9(6), WS-A15-SEQNO 9(2) TO MATCH A15 OUTPUT
      * BAH 2018.1106 ADDED PL-FROM-DATE SCREEN INPUT AND ZERO BAL FROM DATE
      *               WORLD #1346
      * BAH 2019.0201 TEST-DATE-PAID-LAST WAS NOT GETTING SET IN BATCH, 1346A
      * BAH 20200827 REMOVED RESET ON FIRST FIELD
      * BAH 2022.0322 ADDED XLS-M-TRANS-FROM-BRNO, ACCTNO AND DATE #1547
      * BAH 20220824 HARD CODED START
      * BAH 2022.0920 ADDED XLS-M-MISPAY-90-DATE, #1570
      * RMZ 2023.1127 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      * BAH 2023.1130 REMOVED CRNOR2, ADDED LPCRNO, NEEDS FULL LN-REC!
      * BAH 2024.0109 ADDED XLS-M-SECOND-BEN FIELDS #1637
      * RMZ 2024.0116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      * RMZ 2025.0409 CHANGED PROCESSING TO ONLY WRITE DATA TO THE FOLLOWING
      *               4 EXTRACTS: (BANKRUPT, COLLATERAL, F9MISC, DEBITCARD) 
      *               CHANGED REGION-NBR FROM ZEROES TO SPACES IN S35 FILES
      *                TO MATCH A15 FILES. 
      *               THERE ARE "+" SIGNS IN MANY FIELDS IN THE A15 FILES
      *                THAT ARE NOT IN THE S35 COLLATERAL FILE.  
      *               THE FIELDS SHOULD NOT CONTAIN ANY "+" SIGNS. 
      *               DUE TO A BUG IN A15, THE XLTD-TYPE WAS COMMENTED OUT
      *                SO SPACES ARE IN THIS FIELD, REPLACING ZEROES. 
      *               BECAUSE A15 HAS ADDED NEW FIELDS OVER THE YEARS BUT THEY 
      *                WERE NEVER INITIALIZED TO ZEROES, SOME OF THE FIELDS 
      *                IN A15 SHOW SPACES WHILE S35 SHOW ZEROES. THE FIELDS
      *                ARE ALL PIC 9 THAT STILL HAVE SPACES THEM IN A15.
      *                                                            S35Q-197
      *
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBEX/EXFSXLS.CPY".
       COPY "LIBEX/EXFSXLSD.CPY".
       COPY "LIBEX/EXFSXLSF.CPY".
       COPY "LIBEX/EXFSXLSL.CPY".
       COPY "LIBEX/EXFSXLSR.CPY".
       COPY "LIBEX/EXFSXLSM.CPY".
       COPY "LIBEX/EXFSXLSJ.CPY".
       COPY "LIBEX/EXFSXLTA.CPY".
       COPY "LIBEX/EXFSXLTB.CPY".
       COPY "LIBEX/EXFSXLTC.CPY".
       COPY "LIBEX/EXFSXLTD.CPY".
       COPY "LIBEX/EXFSXLTI.CPY".
       COPY "LIBEX/EXFSXLTL.CPY".
       COPY "LIBEX/EXFSXLTP.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBEX/EXFDXLS.CPY".
       COPY "LIBEX/EXFDXLSD.CPY".
       COPY "LIBEX/EXFDXLSF.CPY".
       COPY "LIBEX/EXFDXLSL.CPY".
       COPY "LIBEX/EXFDXLSR.CPY".
       COPY "LIBEX/EXFDXLSM.CPY".
       COPY "LIBEX/EXFDXLSJ.CPY".
       COPY "LIBEX/EXFDXLTA.CPY".
       COPY "LIBEX/EXFDXLTB.CPY".
       COPY "LIBEX/EXFDXLTC.CPY".
       COPY "LIBEX/EXFDXLTD.CPY".
       COPY "LIBEX/EXFDXLTI.CPY".
       COPY "LIBEX/EXFDXLTL.CPY".
       COPY "LIBEX/EXFDXLTP.CPY".
      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/LTEXTR S35 05/01/25-09:58:49 >".

      ******************************************************************
      *
      *     F I L E   S P E C S
      *
      ******************************************************************
       01  XLS-PATH.
           03  XLS-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLS-FILENAME       PIC X(4) VALUE "REPO".
       01  XLSD-PATH.
           03  XLSD-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLSD-FILENAME       PIC X(10) VALUE "DEFICIENCY".
       01  XLSF-PATH.
           03  XLSF-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLSF-FILENAME       PIC X(8) VALUE "FOLLOWUP".
       01  XLSL-PATH.
           03  XLSL-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLSL-FILENAME       PIC X(5) VALUE "LEGAL".
       01  XLSR-PATH.
           03  XLSR-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLSR-FILENAME       PIC X(10) VALUE "RESCHEDULE".
       01  XLSM-PATH.
           03  XLSM-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLSM-FILENAME       PIC X(10) VALUE "F9MISC".
       01  XLSJ-PATH.
           03  XLSJ-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLSJ-FILENAME       PIC X(10) VALUE "JUDGMENT".
       01  XLTA-PATH.
           03  XLTA-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLTA-FILENAME       PIC X(08) VALUE "AUTO_INS".
       01  XLTB-PATH.
           03  XLTB-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLTB-FILENAME       PIC X(08) VALUE "BANKRUPT".
       01  XLTC-PATH.
           03  XLTC-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLTC-FILENAME       PIC X(10) VALUE "COLLATERAL".
       01  XLTD-PATH.
           03  XLTD-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLTD-FILENAME       PIC X(09) VALUE "DEBITCARD".
       01  XLTI-PATH.
           03  XLTI-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLTI-FILENAME       PIC X(05) VALUE "ADDON".
       01  XLTL-PATH.
           03  XLTL-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLTL-FILENAME       PIC X(10) VALUE "LIENHOLDER".
       01  XLTP-PATH.
           03  XLTP-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLTP-FILENAME       PIC X(08) VALUE "SCHEDULE".

       COPY "LIBLP/LP01LSD.CPY".
       COPY "LIBLP/LP01LSD_SQL.CPY".
       COPY "LIBLP/LPWSLSD.CPY".
       COPY "LIBLP/LP01LSF.CPY".
       COPY "LIBLP/LP01LSF_SQL.CPY".
       COPY "LIBLP/LPWSLSF.CPY".
       COPY "LIBLP/LP01LSL.CPY".
       COPY "LIBLP/LP01LSL_SQL.CPY".
       COPY "LIBLP/LPWSLSL.CPY".
       COPY "LIBLP/LP01LSR.CPY".
       COPY "LIBLP/LP01LSR_SQL.CPY".
       COPY "LIBLP/LPWSLSR.CPY".
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
       COPY "LIBLP/LP01LSJ.CPY".
       COPY "LIBLP/LP01LSJ_SQL.CPY".
       COPY "LIBLP/LPWSLSJ.CPY".
       COPY "LIBLP/LP01LSX.CPY".
       COPY "LIBLP/LP01LSX_SQL.CPY".
       COPY "LIBLP/LPWSLSX.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LTA.CPY".
       COPY "LIBLP/LP01LTA_SQL.CPY".
       COPY "LIBLP/LPWSLTA.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTD.CPY".
       COPY "LIBLP/LP01LTD_SQL.CPY".
       COPY "LIBLP/LPWSLTD.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTL.CPY".
       COPY "LIBLP/LP01LTL_SQL.CPY".
       COPY "LIBLP/LPWSLTL.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
      *----------------------------------------------------------------
       01  XLS-KEY       PIC 9(9)     VALUE 0.
       01  XLSD-KEY      PIC 9(9)     VALUE 0.
       01  XLSF-KEY      PIC 9(9)     VALUE 0.
       01  XLSL-KEY      PIC 9(9)     VALUE 0.
       01  XLSR-KEY      PIC 9(9)     VALUE 0.
       01  XLSM-KEY      PIC 9(9)     VALUE 0.
       01  XLSJ-KEY      PIC 9(9)     VALUE 0.
       01  XLTA-KEY      PIC 9(9)     VALUE 0.
       01  XLTL-KEY      PIC 9(9)     VALUE 0.
       01  XLTP-KEY      PIC 9(9)     VALUE 0.
       01  XLTB-KEY      PIC 9(9)     VALUE 0.
       01  XLTC-KEY      PIC 9(9)     VALUE 0.
       01  XLTD-KEY      PIC 9(9)     VALUE 0.
       01  XLTI-KEY      PIC 9(9)     VALUE 0.

       01  WS-BRACCTNO.
           03  WS-OWNBR    PIC 9(4).
           03  WS-NUMBER   PIC 9(6).

       01 WS-A15-ACCTNO.
          03  WS-A15-NUMBER    PIC 9(6).
          03  WS-A15-SEQNO     PIC 9(2).


       EXEC SQL BEGIN DECLARE SECTION END-EXEC.  
       01  QWS-ZBAL                PIC X(1).   
       01  QWS-PL                  PIC X(1).   
       01  QWS-PLACTIVE            PIC X(1).   
       01  QWS-PLTYPE              PIC X(1).   
       01  QWS-PL-FROM-DATE        PIC X(10).  
       01  QWS-DATE-PAID-LAST      PIC X(10).  
       01  QWS-ZBAL-DATE           PIC X(10).  
       01  QWS-JULIAN-BEG          PIC X(10).  
       01  QWS-TEST-DATE-PAID-LAST PIC X(10).  
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-LSLT-CONN-OPEN-SW   PIC X    VALUE "N".
 
      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".

       01  WRITE-REPO-FLAG      PIC X        VALUE SPACES.
       01  STARTING-TIME        PIC X(8)     VALUE SPACES.

       01  HOLD-LN-LS-OWNBR     PIC 9(4)     VALUE 0.
       01  HOLD-LN-LS-ACCTNO    PIC 9(8)     VALUE 0.
       01  HOLD-LN-LS-CLASS     PIC 9(3)     VALUE 0.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  AGE-DATE               PIC 9(8).
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  BEG-CLASS              PIC 9(3).
           03  END-CLASS              PIC 9(3).
           03  PL                     PIC X.
           03  PLTYPE                 PIC X.
           03  PLACTIVE               PIC X.
           03  ZBAL                   PIC X.
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).
           03  COLL                   PIC X.
           03  PL-FROM-DATE           PIC 9(8).
           03  ZBAL-DATE              PIC 9(8).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9999 VALUE 9999.
       01  SV-END-BR           PIC 9999 VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  TEST-DATE-PAID-LAST PIC 9(8) VALUE 0.
      *-----------------------------------------------------------------
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------

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
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(23)    VALUE " ".
           03  H-TITLE          PIC X(28)    VALUE
           "LOAN TRAILER FILE EXTRACTION".
           03  FILLER           PIC X(6)     VALUE " FOR: ".
           03  H-AGEDATE        PIC X(10).

      *-----------------------------------------------------------------

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6)     COMP VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NEW-BRANCH           PIC X        VALUE "N".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  WK-DISPLAY-MESS.
           03  FILLER           PIC X(29)    VALUE
           "BUILDING EXTRACTION FILE....".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X(10).

       01  HOLD-TIME         PIC 9(4).
       01  HOLD-HR-MIN REDEFINES HOLD-TIME.
           03  HOLD-HR       PIC 9(2).
           03  HOLD-MINUTE   PIC 9(2).
       01  HOLD-AM-PM        PIC X(2).


       01  LS-RECORD-COUNT      PIC 9(9)      VALUE 0.
       01  LSD-RECORD-COUNT     PIC 9(9)      VALUE 0.
       01  LSF-RECORD-COUNT     PIC 9(9)      VALUE 0.
       01  LSL-RECORD-COUNT     PIC 9(9)      VALUE 0.
       01  LSR-RECORD-COUNT     PIC 9(9)      VALUE 0.
       01  LSM-RECORD-COUNT     PIC 9(9)      VALUE 0.
       01  LSJ-RECORD-COUNT     PIC 9(9)      VALUE 0.
       01  LTA-RECORD-COUNT     PIC 9(9)      VALUE 0.
       01  LTL-RECORD-COUNT     PIC 9(9)      VALUE 0.
       01  LTB-RECORD-COUNT     PIC 9(9)      VALUE 0.
       01  LTC-RECORD-COUNT     PIC 9(9)      VALUE 0.
       01  LTD-RECORD-COUNT     PIC 9(9)      VALUE 0.
       01  LTI-RECORD-COUNT     PIC 9(9)      VALUE 0.
       01  LTP-RECORD-COUNT     PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(30).
           03  RANGE-SELECTION   PIC X(100).
           03  RANGE-SELNUM     REDEFINES RANGE-SELECTION.
               05  RANGE-DATE PIC 9999/99/99.
               05  FILLER        PIC X(90).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".

       COPY "LIBLP/LPCRNOW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPDEFW.CPY".
       COPY "LIBLP/DEFPOLW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".

       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/LTEXTR_DEF.CPY".
       COPY "LIBSP/LTEXTR_WKS.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/LTEXTR_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE
               XLTL-FILE XLTP-FILE XLS-FILE XLTA-FILE XLTB-FILE
               XLTC-FILE XLTD-FILE XLTI-FILE XLSD-FILE XLSF-FILE
               XLSR-FILE XLSM-FILE XLSL-FILE XLSJ-FILE.

       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTL1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LTA1-FILE.
           PERFORM CLOSE-LSX1-FILE.
           PERFORM CLOSE-LSR1-FILE.
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-LSL1-FILE.
           PERFORM CLOSE-LSJ1-FILE.
           PERFORM CLOSE-LSF1-FILE.
           PERFORM CLOSE-LSD1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               XLTL-FILE XLTP-FILE XLS-FILE XLTA-FILE XLTB-FILE
               XLTC-FILE XLTD-FILE XLTI-FILE XLSD-FILE XLSF-FILE
               XLSR-FILE XLSM-FILE XLSL-FILE XLSJ-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
       MAIN-MODULE SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.
      D    STOP MESS.

       MAIN-ROUTINE.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LTEXTR" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

      * SET THE 3+ YEARS FOR TEST-DATE-PAID-LAST
           MOVE 0 TO TEST-DATE-PAID-LAST.
           IF (PL = "I" OR "O") AND (PL-FROM-DATE NOT = 0)
              MOVE PL-FROM-DATE TO NDTE-DATE
              MOVE 3 TO NDTE-HOLD
              PERFORM INCREMENT-YEARS
              MOVE NDTE-DATE TO TEST-DATE-PAID-LAST.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
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

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "EXTRACTION IN PROGRESS.........", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              BW-PATH-OVERRIDE
                              NO-PATH-OVERRIDE
                              GB-PATH-OVERRIDE
                              LP-PATH-OVERRIDE
                              LSD-PATH-OVERRIDE
                              LSF-PATH-OVERRIDE
                              LSL-PATH-OVERRIDE
                              LSR-PATH-OVERRIDE
                              LSM-PATH-OVERRIDE
                              LSJ-PATH-OVERRIDE
                              LSX-PATH-OVERRIDE
                              LTA-PATH-OVERRIDE
                              LTB-PATH-OVERRIDE
                              LTC-PATH-OVERRIDE
                              LTI-PATH-OVERRIDE
                              LTL-PATH-OVERRIDE
                              LTP-PATH-OVERRIDE
                              LTD-PATH-OVERRIDE.


           PERFORM OPEN-NO1-FILE.
           PERFORM OPEN-LN3-FILE.

      * THIS DOES WORK, JUST HAVE TO LEAR THE MESS BEFORE EXITING
      *    IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE BR-NO TO DISPLAY-MESS-BR
                 MOVE WK-DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.


           PERFORM START-LN3-LSLT-FILE. *> SSP - LN, LSM/LTB/LTC/LTD

       NEXT-LN.
           PERFORM READ-LN3-LSLT-FILE-NEXT. *> EMBED - LN,LSM/LTB/LTC/LTD
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.
       
           IF LN-CLASS > END-CLASS
              GO TO END-OF-GROUP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  POCD - **  IS A DELETED CONTRACT
      *  KEVIN SAYS DON'T SKIP DELETED
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.
    
           IF LN-CLASS < BEG-CLASS
              GO TO NEXT-LN.
       
           IF PL = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-LN.
       
           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.
       
           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "A"
                 IF LN-PLCD NOT = "P"
                    GO TO NEXT-LN.
       
           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "I"
                 IF LN-PLCD NOT = "I"
                    IF LN-PLCD NOT = "W"
                       GO TO NEXT-LN.
       
           IF LN-PLDATE NOT = 0
              IF PLTYPE NOT = "A"
                 IF LN-PLTYPE NOT = PLTYPE
                    GO TO NEXT-LN.

      * TEST P&L FROM DATE, IF LN-PLDATE IS PRIOR TO THIS DATE, SKIP
      * BUT ONLY IF DATE OF LAST PAYMENT IS OLDER THAN +3 YEARS FROM
      * PL-FROM-DATE
      * EXAMPLE: IF PL-FROM-DATE IS 01/01/2010, SKIP CHARGE OFFS FROM 2009
      *          AND PRIOR, AS LONG AS THE DATE PAID LAST IS OLDER THAN
      *          01/01/2013
           IF LN-PLDATE NOT = 0
              IF LN-PLDATE < PL-FROM-DATE
      * NEVER MADE A PAYMENT
                 IF LN-DATE-PAID-LAST = 0
                    GO TO NEXT-LN
                 ELSE
                    IF LN-DATE-PAID-LAST < TEST-DATE-PAID-LAST
                       GO TO NEXT-LN.

           IF ZBAL = "O"
              IF LN-CURBAL NOT = 0 OR
                  LN-INTBAL NOT = 0 OR
                   LN-LCBAL NOT = 0 OR
                    LN-OTHBAL NOT = 0
                      GO TO NEXT-LN.
           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                    IF LN-LCBAL = 0
                       IF LN-OTHBAL = 0
                       GO TO NEXT-LN.

      * TEST ZERO BALANCES FROM DATE, IF PRIOR TO THIS DATE, SKIP
      * IF EQUAL OR GREATER THAN THIS DATE, INCLUDE
           IF LN-POFFDATE NOT = 0
              IF ZBAL-DATE NOT = EXT-JULIAN-BEG
                 IF LN-POFFDATE < ZBAL-DATE
                    GO TO NEXT-LN.

       PROCESS-ACCOUNT.
      * ALWAYS BUILD COLLATERAL FILE
           PERFORM OPEN-LTC1-FILE.
           PERFORM EXTRACT-COLLATERAL-TRAILER.
           PERFORM CLOSE-LTC1-FILE.

      * COLLATERAL ONLY Y/N
           IF COLL NOT = "Y"
      *       PERFORM OPEN-LSX1-FILE
      *       PERFORM EXTRACT-REPO-TRAILER
      *       PERFORM CLOSE-LSX1-FILE

      *       PERFORM OPEN-LSD1-FILE
      *       PERFORM EXTRACT-DEFICIENCY
      *       PERFORM CLOSE-LSD1-FILE

      *       PERFORM OPEN-LSR1-FILE
      *       PERFORM EXTRACT-RESCHEDULE-SCRA
      *       PERFORM CLOSE-LSR1-FILE

      *       PERFORM OPEN-LSF1-FILE
      *       PERFORM EXTRACT-FOLLOWUP
      *       PERFORM CLOSE-LSF1-FILE

      *       PERFORM OPEN-LSL1-FILE
      *       PERFORM EXTRACT-LEGAL
      *       PERFORM CLOSE-LSL1-FILE

      *       PERFORM OPEN-LTA1-FILE
      *       PERFORM EXTRACT-AUTO-INS-TRAILER
      *       PERFORM CLOSE-LTA1-FILE

      *       PERFORM OPEN-LTL1-FILE
      *       PERFORM EXTRACT-LIENHOLDER-TRAILER
      *       PERFORM CLOSE-LTL1-FILE

              PERFORM OPEN-LTB1-FILE
              PERFORM EXTRACT-BANKRUPT-TRAILER
              PERFORM CLOSE-LTB1-FILE

      *       PERFORM OPEN-LTI1-FILE
      *       PERFORM EXTRACT-ADDON-TRAILER
      *       PERFORM CLOSE-LTI1-FILE

              PERFORM OPEN-LSM1-FILE
              PERFORM EXTRACT-F9MISC-TRAILER
              PERFORM CLOSE-LSM1-FILE

      *       PERFORM OPEN-LSJ1-FILE
      *       PERFORM EXTRACT-JUDGMENT-TRAILER
      *       PERFORM CLOSE-LSJ1-FILE

              PERFORM OPEN-LTD1-FILE
              PERFORM EXTRACT-DEBITCARD-TRAILER
              PERFORM CLOSE-LTD1-FILE

      *       IF LN-PAY-SCHLD-FG = "Y"
      *          PERFORM OPEN-LTP1-FILE
      *          PERFORM EXTRACT-PAYMENT-SCHEDULE-TRAILER
      *          PERFORM CLOSE-LTP1-FILE.

           GO TO NEXT-LN.    

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-NO1-FILE.

           MOVE "Y" TO FIRST-TIME.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       EXTRACT-REPO-TRAILER SECTION.
      ******************************************************************
           MOVE SPACES TO XLS-REC.

           MOVE SPACES TO WRITE-REPO-FLAG.
      *    MOVE LN-OWNBR  TO LSX-BRNO.
      *    MOVE LN-ACCTNO TO LSX-ACCTNO.
      *    MOVE 1         TO LSX-SEQNO.
      *    PERFORM READ-LSX1-FILE.
      *    IF IO-BAD
           IF LSX-ACCTNO = 0
              GO TO EXIT-EXTRACT-REPO.

           MOVE "Y" TO WRITE-REPO-FLAG.
           MOVE LN-OWNBR               TO WS-OWNBR.
           MOVE LN-ACCTNO              TO WS-NUMBER.
           MOVE WS-BRACCTNO            TO XLS-BRACCTNO.
           MOVE BR-REGION-NUMBER       TO XLS-REGION-NUMBER.

           MOVE LSX-ACCTNO             TO XLS-ACCTNO.
      *CS TO MATCH A15 ACCOUNT NUMBER
           MOVE LSX-ACCTNO           TO WS-A15-NUMBER.
           MOVE 01                   TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO        TO XLS-ACCTNO.   
           MOVE "X"                    TO XLS-CODE.
           MOVE LSX-SEQNO              TO XLS-SEQNO.

           MOVE LSX-REPODATE           TO XLS-REPODATE.

           MOVE LSX-TITLE-MILEAGE     TO XLS-TITLE-MILEAGE.
           MOVE LSX-VOLUNTARY-FG      TO XLS-VOLUNTARY-FG.
           MOVE LSX-PERFECT           TO XLS-PERFECT.

           MOVE LSX-AUTHORIZED-DATE   TO XLS-AUTHORIZED-DATE.

           MOVE LSX-AUTHORIZED-BY     TO XLS-AUTHORIZED-BY.
           MOVE LSX-REASON            TO XLS-REASON.
           MOVE LSX-X-ASSIGNED-TO     TO XLS-ASSIGNED-TO.

           MOVE LSX-ASSIGNED-DATE     TO XLS-ASSIGNED-DATE.

           MOVE LSX-REPO-CKNO         TO XLS-REPO-CKNO.

           MOVE LSX-REPO-CKDATE       TO XLS-REPO-CKDATE.

           MOVE LSX-REPO-CKAMT        TO XLS-REPO-CKAMT.
           INSPECT XLS-REPO-CKAMT REPLACING LEADING "+" BY " ".
           MOVE LSX-STORED-ADR1       TO XLS-STORED-ADR1.

           MOVE LSX-STORED-CKDATE     TO XLS-STORED-CKDATE.

           MOVE LSX-STORED-CKNO       TO XLS-STORED-CKNO.
           MOVE LSX-STORED-CKAMT      TO XLS-STORED-CKAMT.
           INSPECT XLS-STORED-CKAMT REPLACING LEADING "+" BY " ".

           MOVE LSX-REDEEM-DATE       TO XLS-REDEEM-DATE.

           MOVE LSX-BUREAU-NOTIFIED   TO XLS-BUREAU-NOTIFIED.

           MOVE LSX-BUREAU-DATE       TO XLS-BUREAU-DATE.

           MOVE LSX-BUREAU-BY         TO XLS-BUREAU-BY.

           MOVE LSX-10DAY-DATE        TO XLS-10DAY-DATE.

           MOVE LSX-10DAY-BY          TO XLS-10DAY-BY.
           MOVE LSX-10DAY-COST        TO XLS-10DAY-COST.
           INSPECT XLS-10DAY-COST REPLACING LEADING "+" BY " ".

           MOVE LSX-TITLE-APPL-DATE   TO XLS-TITLE-APPL-DATE.

           MOVE LSX-TITLE-APPL-BY     TO XLS-TITLE-APPL-BY.
           MOVE LSX-TITLE-APPL-COST   TO XLS-TITLE-APPL-COST.
           INSPECT XLS-TITLE-APPL-COST REPLACING LEADING "+" BY " ".
           MOVE LSX-COND-MILEAGE      TO XLS-COND-MILEAGE.
           MOVE LSX-COND-REPORT-Y-N   TO XLS-COND-REPORT-Y-N.
           MOVE LSX-REPAIRS-DESC(1)   TO XLS-REPAIRS-DESC1.
           MOVE LSX-REPAIRS-DESC(2)   TO XLS-REPAIRS-DESC2.
           MOVE LSX-REPAIRS-COST      TO XLS-REPAIRS-COST.
           INSPECT XLS-REPAIRS-COST REPLACING LEADING "+" BY " ".
           MOVE LSX-ASSIGN-TO(1)      TO XLS-ASSIGN-TO1.

           MOVE LSX-ASSIGN-COMPLETE-DATE(1) TO XLS-ASSIGN-COMPLETE1.
           MOVE LSX-ASSIGN-CKDATE(1)        TO XLS-ASSIGN-CKDATE1.

           MOVE LSX-ASSIGN-CKNO(1)    TO XLS-ASSIGN-CKNO1.
           MOVE LSX-ASSIGN-CKAMT(1)   TO XLS-ASSIGN-CKAMT1.
           INSPECT XLS-ASSIGN-CKAMT1 REPLACING LEADING "+" BY " ".
           MOVE LSX-ASSIGN-TO(2)      TO XLS-ASSIGN-TO2.

           MOVE LSX-ASSIGN-COMPLETE-DATE(2) TO XLS-ASSIGN-COMPLETE2.
           MOVE LSX-ASSIGN-CKDATE(2)        TO XLS-ASSIGN-CKDATE2.

           MOVE LSX-ASSIGN-CKNO(2)    TO XLS-ASSIGN-CKNO2.
           MOVE LSX-ASSIGN-CKAMT(2)   TO XLS-ASSIGN-CKAMT2.
           INSPECT XLS-ASSIGN-CKAMT2 REPLACING LEADING "+" BY " ".
           MOVE LSX-OTH-EXP(1)        TO XLS-OTH-EXP1.

           MOVE LSX-OTH-EXP-CKDATE(1) TO XLS-OTH-EXP-CKDATE1.

           MOVE LSX-OTH-EXP-CKNO(1)   TO XLS-OTH-EXP-CKNO1.
           MOVE LSX-OTH-EXP-CKAMT(1)  TO XLS-OTH-EXP-CKAMT1.
           INSPECT XLS-OTH-EXP-CKAMT1 REPLACING LEADING "+" BY " ".
           MOVE LSX-OTH-EXP(2)        TO XLS-OTH-EXP2.

           MOVE LSX-OTH-EXP-CKDATE(2) TO XLS-OTH-EXP-CKDATE2.

           MOVE LSX-OTH-EXP-CKNO(2)   TO XLS-OTH-EXP-CKNO2.
           MOVE LSX-OTH-EXP-CKAMT(2)  TO XLS-OTH-EXP-CKAMT2.
           INSPECT XLS-OTH-EXP-CKAMT2 REPLACING LEADING "+" BY " ".
           MOVE LSX-OTH-EXP(3)        TO XLS-OTH-EXP3.

           MOVE LSX-OTH-EXP-CKDATE(3) TO XLS-OTH-EXP-CKDATE3.

           MOVE LSX-OTH-EXP-CKNO(3)   TO XLS-OTH-EXP-CKNO3.
           MOVE LSX-OTH-EXP-CKAMT(3)  TO XLS-OTH-EXP-CKAMT3.
           INSPECT XLS-OTH-EXP-CKAMT3 REPLACING LEADING "+" BY " ".

           MOVE LSX-INS-RB-REQ-DATE   TO XLS-INS-RB-REQ-DATE.
           MOVE LSX-INS-RB-REC-DATE   TO XLS-INS-RB-REC-DATE.

           MOVE LSX-INS-RB-AMT        TO XLS-INS-RB-AMT.
           INSPECT XLS-INS-RB-AMT REPLACING LEADING "+" BY " ".

           MOVE LSX-RB-REQ-DATE       TO XLS-RB-REQ-DATE.
           MOVE LSX-RB-REC-DATE       TO XLS-RB-REC-DATE.

           MOVE LSX-RB-AMT            TO XLS-RB-AMT.
           INSPECT XLS-RB-AMT REPLACING LEADING "+" BY " ".

           MOVE LSX-SIC-INS-FILE-DATE TO XLS-SIC-INS-FILE-DATE.

           MOVE LSX-SIC-INS-PAID-DATE TO XLS-SIC-INS-PAID-DATE.

           MOVE LSX-SIC-INS-PAID-AMT  TO XLS-SIC-INS-PAID-AMT.
           INSPECT XLS-SIC-INS-PAID-AMT REPLACING LEADING "+" BY " ".
           MOVE LSX-BOOK-VALUE        TO XLS-BOOK-VALUE.
           MOVE LSX-SOLD-TO(1)        TO XLS-SOLD-TO1.
           MOVE LSX-SOLD-TO(2)        TO XLS-SOLD-TO2.
           MOVE LSX-SOLD-TO(3)        TO XLS-SOLD-TO3.

           MOVE LSX-SOLD-TO-DATE      TO XLS-SOLD-TO-DATE.

           MOVE LSX-SOLD-TO-AMT       TO XLS-SOLD-TO-AMT.
           INSPECT XLS-SOLD-TO-AMT REPLACING LEADING "+" BY " ".
           MOVE LSX-DLNO              TO XLS-DLNO.
           MOVE LSX-DLR-RSV-CKNO      TO XLS-DLR-RSV-CKNO.
           MOVE LSX-DLR-RSV-CKAMT     TO XLS-DLR-RSV-CKAMT.
           INSPECT XLS-DLR-RSV-CKAMT REPLACING LEADING "+" BY " ".
           MOVE LSX-PL-EXPENSE        TO XLS-PL-EXPENSE.
           INSPECT XLS-PL-EXPENSE REPLACING LEADING "+" BY " ".
           MOVE LSX-DEFICIENCY-Y-N    TO XLS-DEFICIENCY-Y-N.
           MOVE LSX-NET-BAL-DUE       TO XLS-NET-BAL-DUE.
           INSPECT XLS-NET-BAL-DUE REPLACING LEADING "+" BY " ".
           MOVE LSX-DEFICIENCY-BAL    TO XLS-DEFICIENCY-BAL.
           INSPECT XLS-DEFICIENCY-BAL REPLACING LEADING "+" BY " ".
           MOVE LSX-AUCTION-EXPENSE   TO XLS-AUCTION-EXPENSE.
           INSPECT XLS-AUCTION-EXPENSE REPLACING LEADING "+" BY " ".
           MOVE LSX-REASON-CODE       TO XLS-REASON-CODE.
           MOVE LSX-TRIM              TO XLS-TRIM.
           MOVE LSX-DOORS             TO XLS-DOORS.
           MOVE LSX-COLOR             TO XLS-COLOR.
           MOVE LSX-CYLINDERS         TO XLS-CYLINDERS.
           MOVE LSX-AUTO-TRANS        TO XLS-AUTO-TRANS.
           MOVE LSX-AC                TO XLS-AC.
           MOVE LSX-SROOF             TO XLS-SROOF.
           MOVE LSX-LEATHER           TO XLS-LEATHER.
           MOVE LSX-4WD               TO XLS-4WD.
           MOVE LSX-DIESEL            TO XLS-DIESEL.
           MOVE LSX-DUAL-AIR          TO XLS-DUAL-AIR.
           MOVE LSX-EXT-CAB           TO XLS-EXT-CAB.
           MOVE LSX-AGENT-PHONE       TO XLS-AGENT-PHONE.
           MOVE LSX-AGENT-FAX         TO XLS-AGENT-FAX.
           MOVE LSX-STORED-ADR2       TO XLS-STORED-ADR2.
           MOVE LSX-X-STORED-CITY       TO XLS-STORED-CITY.
           MOVE LSX-X-STORED-STATE      TO XLS-STORED-STATE.
           MOVE LSX-X-STORED-ZIP        TO XLS-STORED-ZIP.
           MOVE LSX-INS-CLAIM-PENDING TO XLS-INS-CLAIM-PENDING.
           MOVE LSX-WARRANTY-AVAILABLE   TO XLS-WARRANTY-AVAILABLE.
           MOVE LSX-INS-REBATE-REQUESTED TO XLS-INS-REBATE-REQUESTED.
           MOVE LSX-EXTRACTED         TO XLS-EXTRACTED.

           MOVE LSX-TITLE-SENT-DATE   TO XLS-TITLE-SENT-DATE.

           MOVE LSX-DISPOSAL-HOLD TO YLS-DISPOSAL-HOLD.
           MOVE LSX-NEXT-HLD-RVW-DATE TO YLS-NEXT-HLD-RVW-DATE.

           MOVE LSX-CASE-ID       TO YLS-CASE-ID.
           MOVE LSX-COLLATERAL-LOCATION TO YLS-COLLATERAL-LOCATION.
           MOVE LSX-VIN-MATCH     TO YLS-VIN-MATCH.
           MOVE LSX-PERSONALS     TO YLS-PERSONALS.
           MOVE LSX-KEYS          TO YLS-KEYS.
           MOVE LSX-DRIVES        TO YLS-DRIVES.
           MOVE LSX-DMG           TO YLS-DMG.
           MOVE LSX-Y-ASSIGNED-TO TO YLS-ASSIGNED-TO.
           MOVE LSX-SUPPLIER-ID   TO YLS-SUPPLIER-ID.
           MOVE LSX-TRANSPORT-EMAIL TO YLS-TRANSPORT-EMAIL.
           MOVE LSX-STORED-LOCATION TO YLS-STORED-LOCATION.
           MOVE LSX-STORED-ADDRESS  TO YLS-STORED-ADDRESS.
           MOVE LSX-Y-STORED-CITY     TO YLS-STORED-CITY.
           MOVE LSX-Y-STORED-STATE    TO YLS-STORED-STATE.
           MOVE LSX-Y-STORED-ZIP      TO YLS-STORED-ZIP.
           MOVE LSX-AGENCY-NAME     TO YLS-AGENCY-NAME.
           MOVE LSX-AGENCY-ADDRESS  TO YLS-AGENCY-ADDRESS.
           MOVE LSX-AGENCY-CITY     TO YLS-AGENCY-CITY.
           MOVE LSX-AGENCY-STATE    TO YLS-AGENCY-STATE.
           MOVE LSX-AGENCY-ZIP      TO YLS-AGENCY-ZIP.
           MOVE LSX-AGENCY-PHONE    TO YLS-AGENCY-PHONE.

           MOVE X"0D"                  TO XLS-CR.
           MOVE X"0A"                  TO XLS-LF.

           ADD 1 TO XLS-KEY.
           PERFORM WRITE-XLS-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO LS-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

       EXIT-EXTRACT-REPO.
           EXIT.

      ******************************************************************
       EXTRACT-DEFICIENCY SECTION.
      ******************************************************************
           MOVE SPACES TO XLSD-REC.

      *    MOVE LN-OWNBR  TO LSD-BRNO.
      *    MOVE LN-ACCTNO TO LSD-ACCTNO.
      *    MOVE 1         TO LSD-SEQNO.
      *    PERFORM READ-LSD1-FILE.
      *    IF IO-BAD
           IF LSD-ACCTNO = 0
              GO TO EXIT-EXTRACT-DEFICIENCY.

           MOVE LN-OWNBR               TO WS-OWNBR.
           MOVE LN-ACCTNO              TO WS-NUMBER.
           MOVE WS-BRACCTNO            TO XLS-D-BRACCTNO.
      *    MOVE LN-ACCTNO              TO XLS-D-ACCTNO.
      *CS TO MATCH A15 ACCOUNT NUMBER
           MOVE LN-ACCTNO            TO WS-A15-NUMBER.
           MOVE 01                   TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO        TO XLS-D-ACCTNO.
           MOVE "D"                  TO XLS-D-CODE.
           MOVE LSD-SEQNO            TO XLS-D-SEQNO.

           MOVE LSD-STOLEN-DATE      TO XLS-D-STOLEN-DATE.
           MOVE LSD-GAP-INS-PAID     TO XLS-D-GAP-INS-PAID.
           MOVE LSD-OTHER-EXPENSE    TO XLS-D-OTHER-EXPENSE.
           MOVE LSD-MISC-AMT-EXPENSE TO XLS-D-MISC-AMT-EXPENSE.
           MOVE LSD-MISC-DESC-EXPENSE TO XLS-D-MISC-DESC-EXPENSE.
           MOVE LSD-TOT-EXPENSE      TO XLS-D-TOT-EXPENSE.
           MOVE LSD-INT-REB          TO XLS-D-INT-REB.
           MOVE LSD-CPP-REB          TO XLS-D-CPP-REB.
           MOVE LSD-GAP-REB          TO XLS-D-GAP-REB.
           MOVE LSD-WARRANTY-REB     TO XLS-D-WARRANTY-REB.
           MOVE LSD-MISC-AMT-CREDIT  TO XLS-D-MISC-AMT-CREDIT.
           MOVE LSD-MISC-DESC-CREDIT TO XLS-D-MISC-DESC-CREDIT.
           MOVE LSD-TOT-CREDITS      TO XLS-D-TOT-CREDITS.
           MOVE LSD-PAID-TO-OTHER    TO XLS-D-PAID-TO-OTHER.
           MOVE LSD-AMT-FROM-OWING   TO XLS-D-AMT-FROM-OWING.
           MOVE LSD-AMT-OF-SURPLUS   TO XLS-D-AMT-OF-SURPLUS.
           MOVE LSD-AMT-OF-DEFICIENCY   TO XLS-D-AMT-OF-DEFICIENCY.
           MOVE LSD-SEND-FMNO           TO XLS-D-SEND-FMNO.

           MOVE LSD-INSURANCE-PROCEEDS  TO XLS-D-INSURANCE-PROCEEDS.

           MOVE LSD-DATE-CHECK-RECV  TO XLS-D-DATE-CHECK-RECV.
           MOVE LSD-INSURANCE-CHECK  TO XLS-D-INSURANCE-CHECK.
           MOVE LSD-WHO-ADJUSTED     TO XLS-D-WHO-ADJUSTED.
           MOVE LSD-INS-COMPANY      TO XLS-D-INS-COMPANY.
           MOVE LSD-SALES-DATE       TO XLS-D-SALES-DATE.
           MOVE LSD-AUCTION          TO XLS-D-AUCTION.
           MOVE LSD-AUCTN-TIME TO HOLD-TIME.
           IF HOLD-HR = 00
              MOVE 12 TO HOLD-HR.
           IF HOLD-HR > 11
              MOVE "PM" TO HOLD-AM-PM
           ELSE
              MOVE "AM" TO HOLD-AM-PM.
           IF HOLD-HR > 12
              SUBTRACT 12 FROM HOLD-HR.

           MOVE HOLD-TIME             TO XLS-D-AUCTN-TIME.
           MOVE HOLD-AM-PM            TO XLS-D-AUCTN-TIME-AMPM.
           MOVE LSD-AUCTN-ADR        TO XLS-D-AUCTN-ADR.
           MOVE LSD-AUCTN-CITY       TO XLS-D-AUCTN-CITY.
           MOVE LSD-AUCTN-STATE      TO XLS-D-AUCTN-STATE.
           MOVE LSD-AUCTN-ZIP        TO XLS-D-AUCTN-ZIP.
           MOVE LSD-AUCTN-PHONE      TO XLS-D-AUCTN-PHONE.
           MOVE LSD-REPO-CHARGE      TO XLS-D-REPO-CHARGE.
           MOVE LSD-STORAGE-CHARGE   TO XLS-D-STORAGE-CHARGE.
           MOVE LSD-REPAIR-CHARGE    TO XLS-D-REPAIR-CHARGE.
           MOVE LSD-OTHER-CHARGE     TO XLS-D-OTHER-CHARGE.
           MOVE LSD-STORAGE-RATE      TO XLS-D-STORAGE-RATE.
           MOVE LSD-EST-REBATE       TO XLS-D-EST-REBATE.
           MOVE LSD-MINIMUM-BID      TO XLS-D-MINIMUM-BID.
           MOVE LSD-MAY-REINSTATE    TO XLS-D-MAY-REINSTATE.
           MOVE LSD-MAY-NOT-REINSTATE TO XLS-D-MAY-NOT-REINSTATE.
           MOVE LSD-IN-PAST-12MONTHS  TO XLS-D-IN-PAST-12MONTHS.
           MOVE LSD-2X-DURING-CONTRACT TO XLS-D-2X-DURING-CONTRACT.
           MOVE LSD-OTHER             TO XLS-D-OTHER.
           MOVE LSD-OTHER-COMMENTS1   TO XLS-D-OTHER-COMMENTS1.
           MOVE LSD-OTHER-COMMENTS2   TO XLS-D-OTHER-COMMENTS2.
           MOVE LSD-CA-PYMTS-AS-AGREED TO XLS-D-CA-PYMTS-AS-AGREED.
           MOVE LSD-CA-KEEP-INS       TO XLS-D-CA-KEEP-INS.
           MOVE LSD-CA-KEEP-LIEN-FREE TO XLS-D-CA-KEEP-LIEN-FREE.
           MOVE LSD-CA-OTHER          TO XLS-D-CA-OTHER.
           MOVE LSD-CA-OTHER-DESC     TO XLS-D-CA-OTHER-DESC.
           MOVE LSD-CURE              TO XLS-D-CURE.

      *NEED NOTICE FIELDS TO CALCULATE THE FOLLOWING
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
      *    PERFORM READ-SP1-FILE.
           PERFORM ARRAYSP-READ.
           IF IO-FG NOT = 0
             GO TO IO-ERROR.

      *    MOVE LN-OWNBR TO NO-BRNO.
      *    MOVE LN-ACCTNO TO NO-ACCTNO.
      *    PERFORM READ-NO1-FILE.
      *    IF IO-FG NOT = 0

           IF NO-ACCTNO = 0
              MOVE LN-MAKERCD(1) TO CRNO-MAKERCD
              MOVE AGE-DATE      TO CRNO-PAYDATE
              MOVE "N"           TO CRNO-LPREC-FG
              PERFORM CREATE-NOREC.

           COMPUTE XLS-D-CALC-PAST-DUE-PRIN = NO-TDUEAMT - NO-TDUELC.
           COMPUTE XLS-D-CALC-INT-DUE = NO-LCHG-INTDUE - NO-TDUELC.
           MOVE NO-TDUELC TO XLS-D-CALC-LC-DUE.
           COMPUTE XLS-D-CALC-TOT-TO-REINSTATE =
              NO-TDUEAMT + LSD-REPO-CHARGE + LSD-STORAGE-CHARGE +
                 LSD-REPAIR-CHARGE + LSD-OTHER-CHARGE.
           COMPUTE XLS-D-CALC-PMTS-X-2 =  NO-SCHED-PAYMENT * 2.
           MOVE NO-NXDUE-DATE    TO XLS-D-CALC-NXDUE-AFT-REPO.
           MOVE NO-SCHED-PAYMENT TO XLS-D-CALC-NXT-SCHEDULED-AMT.
           MOVE NO-NXDUE-DATE TO NDTE-DATE.
           MOVE SP-LCGRACE    TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE        TO XLS-D-CALC-NXTDUE-LC-AFT-REPO.

           COMPUTE XLS-D-CALC-REDEEM-AMT =
             (LN-CURBAL + LSD-REPO-CHARGE + LSD-STORAGE-CHARGE +
               LSD-REPAIR-CHARGE + LSD-OTHER-CHARGE +
                NO-LCHG-INTDUE) - LSD-EST-REBATE.

           MOVE X"0D"                  TO XLS-D-CR.
           MOVE X"0A"                  TO XLS-D-LF.

           ADD 1 TO XLSD-KEY.
           PERFORM WRITE-XLSD-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO LSD-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

       EXIT-EXTRACT-DEFICIENCY.
           EXIT.

      ******************************************************************
       EXTRACT-RESCHEDULE-SCRA SECTION.
      ******************************************************************
           MOVE SPACES TO XLSR-REC.

      *    MOVE LN-OWNBR  TO LSR-BRNO.
      *    MOVE LN-ACCTNO TO LSR-ACCTNO.
      *    MOVE 1         TO LSR-SEQNO.
      *    PERFORM READ-LSR1-FILE.
      *    IF IO-BAD
           IF LSR-ACCTNO = 0
              GO TO EXIT-EXTRACT-RESCHEDULE-SCRA.

           MOVE LN-OWNBR               TO WS-OWNBR.
           MOVE LN-ACCTNO              TO WS-NUMBER.
           MOVE WS-BRACCTNO            TO XLS-R-BRACCTNO.
      *    MOVE LN-ACCTNO              TO XLS-R-ACCTNO.
      *CS TO MATCH A15 ACCOUNT NUMBER
           MOVE LN-ACCTNO            TO WS-A15-NUMBER.
           MOVE 01                   TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO        TO XLS-R-ACCTNO.
           MOVE "R"                    TO XLS-R-CODE.
           MOVE LSR-SEQNO               TO XLS-R-SEQNO.

           MOVE LSR-SCRA-ACTIVE     TO XLS-R-SCRA-ACTIVE.
           MOVE LSR-SCRA-START-DATE TO XLS-R-SCRA-START-DATE.
           MOVE LSR-SCRA-END-DATE   TO XLS-R-SCRA-END-DATE.
           MOVE LSR-SCRA-BORROWER TO XLS-R-SCRA-BORROWER.
           MOVE LSR-CURRENT-RESCHED-DATE TO XLS-R-CURRENT-RESCHED-DATE.

           MOVE LSR-CURRENT-REASON-CODE TO XLS-R-CURRENT-REASON-CODE.
           MOVE LSR-INITIAL-RESCHED-DATE TO XLS-R-INITIAL-RESCHED-DATE.
           MOVE LSR-INITIAL-REASON-CODE TO XLS-R-INITIAL-REASON-CODE.
           MOVE LSR-INITIAL-TDR-DATE    TO XLS-R-INITIAL-TDR-DATE.
           MOVE LSR-INITIAL-TDR-REASON  TO XLS-R-INITIAL-TDR-REASON.

           MOVE LSR-CURRENT-TDR-DATE TO XLS-R-CURRENT-TDR-DATE.
           MOVE LSR-CURRENT-TDR-REASON TO XLS-R-CURRENT-TDR-REASON.

           MOVE X"0D"                  TO XLS-R-CR.
           MOVE X"0A"                  TO XLS-R-LF.

           ADD 1 TO XLSR-KEY.
           PERFORM WRITE-XLSR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO LSR-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

       EXIT-EXTRACT-RESCHEDULE-SCRA.
           EXIT.

      ******************************************************************
       EXTRACT-FOLLOWUP SECTION.
      ******************************************************************
           MOVE SPACES TO XLSF-REC.

      *    MOVE LN-OWNBR  TO LSF-BRNO.
      *    MOVE LN-ACCTNO TO LSF-ACCTNO.
      *    MOVE 1         TO LSF-SEQNO.
      *    PERFORM READ-LSF1-FILE.
      *    IF IO-BAD
           IF LSF-ACCTNO = 0
              GO TO EXIT-EXTRACT-FOLLOWUP.

           MOVE LN-OWNBR               TO WS-OWNBR.
           MOVE LN-ACCTNO              TO WS-NUMBER.
           MOVE WS-BRACCTNO            TO XLS-F-BRACCTNO.
      *    MOVE LN-ACCTNO              TO XLS-F-ACCTNO.
      *CS TO MATCH A15 ACCOUNT NUMBER
           MOVE LN-ACCTNO            TO WS-A15-NUMBER.
           MOVE 01                   TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO        TO XLS-F-ACCTNO.
           MOVE "F"                    TO XLS-F-CODE.
           MOVE LSF-SEQNO               TO XLS-F-SEQNO.

           MOVE LSF-SKIP-START-DATE  TO XLS-F-SKIP-START-DATE.
           MOVE LSF-SKIP-END-DATE    TO XLS-F-SKIP-END-DATE.

           MOVE LSF-OFR-START-DATE  TO XLS-F-OFR-START-DATE.
           MOVE LSF-OFR-END-DATE    TO XLS-F-OFR-END-DATE.

           MOVE LSF-IMPOUND-START-DATE  TO XLS-F-IMPOUND-START-DATE.
           MOVE LSF-IMPOUND-END-DATE    TO XLS-F-IMPOUND-END-DATE.

           MOVE LSF-BNKRPT-START-DATE  TO XLS-F-BNKRPT-START-DATE.
           MOVE LSF-BNKRPT-END-DATE    TO XLS-F-BNKRPT-END-DATE.

           MOVE LSF-INSUR-START-DATE  TO XLS-F-INSUR-START-DATE.
           MOVE LSF-INSUR-END-DATE    TO XLS-F-INSUR-END-DATE.

           MOVE LSF-COLLECT-START-DATE  TO XLS-F-COLLECT-START-DATE.
           MOVE LSF-COLLECT-END-DATE    TO XLS-F-COLLECT-END-DATE.

           MOVE X"0D"                  TO XLS-F-CR.
           MOVE X"0A"                  TO XLS-F-LF.

           ADD 1 TO XLSF-KEY.
           PERFORM WRITE-XLSF-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO LSF-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

       EXIT-EXTRACT-FOLLOWUP.
           EXIT.

      ******************************************************************
       EXTRACT-LEGAL SECTION.
      ******************************************************************
           MOVE SPACES TO XLSL-REC.

      *    MOVE LN-OWNBR  TO LSL-BRNO.
      *    MOVE LN-ACCTNO TO LSL-ACCTNO.
      *    MOVE 1         TO LSL-SEQNO.
      *    PERFORM READ-LSL1-FILE.
      *    IF IO-BAD
           IF LSL-ACCTNO = 0
              GO TO EXIT-EXTRACT-LEGAL.

           MOVE LN-OWNBR               TO WS-OWNBR.
           MOVE LN-ACCTNO              TO WS-NUMBER.
           MOVE WS-BRACCTNO            TO XLS-L-BRACCTNO.
      *    MOVE LN-ACCTNO              TO XLS-L-ACCTNO.
      *CS TO MATCH A15 ACCOUNT NUMBER 99999901
           MOVE LN-ACCTNO              TO WS-A15-NUMBER.
           MOVE 01                     TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO          TO XLS-L-ACCTNO.
           MOVE "L"                    TO XLS-L-CODE.
           MOVE LSL-SEQNO              TO XLS-L-SEQNO.


           MOVE LSL-DATE-SUIT-AUTH  TO XLS-L-DATE-SUIT-AUTH.

           MOVE LSL-DATE-SUIT-AUTH-BY   TO XLS-L-DATE-SUIT-AUTH-BY.
           MOVE LSL-ATTORNEY-NET-ASSIGN TO XLS-L-ATTORNEY-NET-ASSIGN.
           MOVE LSL-LOCAL-ATTORNEY      TO XLS-L-LOCAL-ATTORNEY       
           MOVE LSL-ATTORNEY-ADDRESS    TO XLS-L-ATTORNEY-ADDRESS.
           MOVE LSL-ATTORNEY-PHONE      TO XLS-L-ATTORNEY-PHONE.

           MOVE LSL-DATE-ASSIGNED  TO XLS-L-DATE-ASSIGNED.

           MOVE LSL-ATTORNEY-ACCTNO       TO XLS-L-ATTORNEY-ACCTNO.
           MOVE LSL-PRIN-BAL-SENT         TO XLS-L-PRIN-BAL-SENT.

           MOVE LSL-RECEIVED-ACKNOW-DATE  TO XLS-L-RECEIVED-ACKNOW.

           MOVE LSL-ATTORNEY-LETTER-SENT-DATE TO
                                        XLS-L-ATTORNEY-LETTER-SENT.

           MOVE LSL-HOLD-DATE               TO XLS-L-HOLD-DATE.
           MOVE LSL-RESPONSE-RECEIVED-DATE  TO XLS-L-RESPONSE-RECEIVED.

           MOVE LSL-SUIT-DATE         TO XLS-L-SUIT-DATE.
           MOVE LSL-SUIT-FILED-DATE   TO XLS-L-SUIT-FILED.
           MOVE LSL-SERVICE-DATE      TO XLS-L-SERVICE-DATE.
           MOVE LSL-CUSTOMER-SERVED   TO XLS-L-CUSTOMER-SERVED.
           MOVE LSL-ANSWER-FILED-DATE TO XLS-L-ANSWER-FILED.

           MOVE LSL-COURT-DATE        TO XLS-L-COURT-DATE.

           MOVE LSL-JUDGMENT-TYPE TO XLS-L-JUDGMENT-TYPE.

           MOVE LSL-JUDGMENT-OBTAINED-DATE TO XLS-L-JUDGMENT-OBTAINED.

           MOVE LSL-JUDGMENT-COPY-FILED TO XLS-L-JUDGMENT-COPY-FILED.
           MOVE LSL-JUDGMENT-AMT          TO XLS-L-JUDGMENT-AMT         
           MOVE LSL-JUDGMENT-PRIN         TO XLS-L-JUDGMENT-PRIN        
           MOVE LSL-POST-JUDG-INT-RATE    TO XLS-L-POST-JUDG-INT-RATE   
           MOVE LSL-JUDG-INT-AMT          TO XLS-L-JUDG-INT-AMT         
           MOVE LSL-ATTORNEY-FEES-ALLOWED TO
                                           XLS-L-ATTORNEY-FEES-ALLOWED
           MOVE LSL-OTHER-FEES-ALLOWED    TO XLS-L-OTHER-FEES-ALLOWED   
           MOVE LSL-JUDG-INT-RATE         TO XLS-L-JUDG-INT-RATE        
           MOVE LSL-ASSET-USED-IN-SUIT    TO XLS-L-ASSET-USED-IN-SUIT   

           MOVE LSL-GARN-FILED-DATE         TO XLS-L-GARN-FILED-DATE.
           MOVE LSL-GARN-TERM-DAYS          TO XLS-L-GARN-TERM-DAYS.
           MOVE LSL-INTERROGATORY-SENT-DATE TO XLS-L-INTERROGATORY-SENT.
           MOVE LSL-INTERROGATORY-ANSW-DATE TO XLS-L-INTERROGATORY-ANSW.
           MOVE LSL-FIFA-SENT-DATE          TO XLS-L-FIFA-SENT.
           MOVE LSL-FIFA-ANSWERED-DATE      TO XLS-L-FIFA-ANSWERED.
           MOVE LSL-DATE-TO-RECV-PAY        TO XLS-L-DATE-TO-RECV-PAY.
           MOVE LSL-FOLLOWUP-DATE           TO XLS-L-FOLLOWUP-DATE.
           MOVE LSL-FIFA-RENEWED-DATE       TO XLS-L-FIFA-RENEWED.
           MOVE LSL-PAYS-RECV-ATTORNEY      TO XLS-L-PAYS-RECV-ATTORNEY. 
           MOVE LSL-DATE-CLOSED             TO XLS-L-DATE-CLOSED.
           MOVE LSL-REASON-CLOSED           TO XLS-L-REASON-CLOSED.
           
           MOVE X"0D"                  TO XLS-L-CR.
           MOVE X"0A"                  TO XLS-L-LF.
           ADD 1 TO XLSL-KEY.
           PERFORM WRITE-XLSL-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO LSL-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

       EXIT-EXTRACT-LEGAL.
           EXIT.


      ******************************************************************
       EXTRACT-AUTO-INS-TRAILER SECTION.
      ******************************************************************

           MOVE SPACES TO XLTA-REC.

      *    MOVE LN-OWNBR  TO LTA-BRNO.
      *    MOVE LN-ACCTNO TO LTA-ACCTNO.
      *    MOVE 1         TO LTA-SEQNO.
      *    PERFORM READ-LTA1-FILE.
      *    IF IO-BAD
           IF LTA-ACCTNO = 0
              GO TO EXIT-EXTRACT-AUTO-INS.

           MOVE LTA-DLR-REQ     TO XLTA-DLR-REQ.
           MOVE LTA-INS-TYPE    TO XLTA-INS-TYPE.
           MOVE LTA-INS-CO      TO XLTA-INS-CO.
           MOVE LTA-POLICY-NO   TO XLTA-POLICY-NO.
           MOVE LTA-AGENCY      TO XLTA-AGENCY.
           MOVE LTA-AGENT       TO XLTA-AGENT.

           MOVE LTA-ADR1        TO XLTA-ADR1.
           MOVE LTA-ADR2        TO XLTA-ADR2.
           MOVE LTA-CITY        TO XLTA-CITY.
           MOVE LTA-STATE       TO XLTA-STATE.
           MOVE LTA-ZIP         TO XLTA-ZIP.
           MOVE LTA-PHONE       TO XLTA-PHONE.

           MOVE LTA-EFF-DATE    TO XLTA-EFF-DATE.
           MOVE LTA-EXP-DATE    TO XLTA-EXP-DATE.
           MOVE LTA-CAN-DATE    TO XLTA-CAN-DATE.
           MOVE LTA-CONF-DATE   TO XLTA-CONF-DATE.

           MOVE LTA-LAST-FORM   TO XLTA-LAST-FORM.

           MOVE LTA-COV-TYPE    TO XLTA-COV-TYPE.
           MOVE LTA-BIL-COV     TO XLTA-BIL-COV.
           INSPECT XLTA-BIL-COV REPLACING LEADING "+" BY " ".
           MOVE LTA-PDL-COV     TO XLTA-PDL-COV.
           INSPECT XLTA-PDL-COV REPLACING LEADING "+" BY " ".
           MOVE LTA-PIP-COV     TO XLTA-PIP-COV.
           INSPECT XLTA-PIP-COV REPLACING LEADING "+" BY " ".
           MOVE LTA-UMB-COV     TO XLTA-UMB-COV.
           INSPECT XLTA-UMB-COV REPLACING LEADING "+" BY " ".
           MOVE LTA-COL-COV     TO XLTA-COL-COV.
           INSPECT XLTA-COL-COV REPLACING LEADING "+" BY " ".
           MOVE LTA-LOSS-PAYEE  TO XLTA-LOSS-PAYEE.
           MOVE LTA-COM-COV     TO XLTA-COM-COV.
           INSPECT XLTA-COM-COV REPLACING LEADING "+" BY " ".

           MOVE LTA-TRACK-ONLY TO XLTA-TRACK-ONLY.
           MOVE LTA-INSURANCE TO XLTA-INSURANCE.
           MOVE LTA-TRACK-MONTHS TO XLTA-TRACK-MONTHS.
           MOVE LTA-INS-MONTHS TO XLTA-INS-MONTHS.
           MOVE LTA-CPP-MONTHS TO XLTA-CPP-MONTHS.

      *-----------------------------------------------------------------

           MOVE LN-OWNBR         TO WS-OWNBR.
           MOVE LN-ACCTNO        TO WS-NUMBER.
           MOVE WS-BRACCTNO      TO XLTA-BRACCTNO.
           MOVE BR-REGION-NUMBER TO XLTA-REGION-NUMBER.

      *    MOVE LTA-ACCTNO        TO XLTA-ACCTNO.
      *CS TO MATCH A15 ACCOUNT NUMBER
           MOVE LTA-ACCTNO           TO WS-A15-NUMBER.
           MOVE 01                   TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO        TO XLTA-ACCTNO.
           MOVE "A"                  TO XLTA-CODE.
           MOVE 0                    TO XLTA-SEQNO XLTA-TYPE.

           MOVE X"0D" TO XLTA-CR.
           MOVE X"0A" TO XLTA-LF.

           ADD 1 TO XLTA-KEY.
           PERFORM WRITE-XLTA-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO LTA-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

       EXIT-EXTRACT-AUTO-INS.
           EXIT.

      ******************************************************************
       EXTRACT-LIENHOLDER-TRAILER SECTION.
      ******************************************************************

      * EXTRACT COLLATERAL LIENHOLDER TRAILER, P2 COLLATERAL

           MOVE SPACES TO XLTL-REC.

      *    MOVE LN-OWNBR  TO LTL-BRNO.
      *    MOVE LN-ACCTNO TO LTL-ACCTNO.
      *    MOVE 1         TO LTL-SEQNO.
      *    PERFORM READ-LTL1-FILE.
      *    IF IO-BAD
           IF LTL-ACCTNO = 0
              GO TO EXIT-EXTRACT-LIENHOLDER.

           MOVE LTL-LIEN-TYPE(1)   TO XLTL-LIEN-1-TYPE.
           MOVE LTL-LIEN-NAME(1)   TO XLTL-LIEN-1-NAME.
           MOVE LTL-LIEN-ADR(1)    TO XLTL-LIEN-1-ADR.
           MOVE LTL-LIEN-CITY(1)   TO XLTL-LIEN-1-CITY.
           MOVE LTL-LIEN-STATE(1)  TO XLTL-LIEN-1-STATE.
           MOVE LTL-LIEN-ZIP(1)    TO XLTL-LIEN-1-ZIP.
           MOVE LTL-LIEN-DATE(1)   TO XLTL-LIEN-1-DATE.
           MOVE LTL-TITLE-RCV-DATE TO XLTL-TITLE-RCV-DATE.
           MOVE LTL-TITLE-LIEN-DATE TO XLTL-TITLE-LIEN-DATE.

           MOVE LTL-TITLE-NO TO XLTL-TITLE-NO.
           MOVE LTL-LIEN-LETTER-SENT TO XLTL-LIEN-LETTER-SENT.

           MOVE LTL-LIEN-TYPE(2)   TO XLTL-LIEN-2-TYPE.
           MOVE LTL-LIEN-NAME(2)   TO XLTL-LIEN-2-NAME.
           MOVE LTL-LIEN-ADR(2)    TO XLTL-LIEN-2-ADR.
           MOVE LTL-LIEN-CITY(2)   TO XLTL-LIEN-2-CITY.
           MOVE LTL-LIEN-STATE(2)  TO XLTL-LIEN-2-STATE.
           MOVE LTL-LIEN-ZIP(2)    TO XLTL-LIEN-2-ZIP.
           MOVE LTL-LIEN-DATE(2)   TO XLTL-LIEN-2-DATE.

           MOVE LTL-LIEN-TYPE(3)   TO XLTL-LIEN-3-TYPE.
           MOVE LTL-LIEN-NAME(3)   TO XLTL-LIEN-3-NAME.
           MOVE LTL-LIEN-ADR(3)    TO XLTL-LIEN-3-ADR.
           MOVE LTL-LIEN-CITY(3)   TO XLTL-LIEN-3-CITY.
           MOVE LTL-LIEN-STATE(3)  TO XLTL-LIEN-3-STATE.
           MOVE LTL-LIEN-ZIP(3)    TO XLTL-LIEN-3-ZIP.
           MOVE LTL-LIEN-DATE(3)   TO XLTL-LIEN-3-DATE.

           MOVE LTL-LIEN-COMMENT1     TO XLTL-LIEN-4-COMMENT1.
           MOVE LTL-LIEN-COMMENT2     TO XLTL-LIEN-4-COMMENT2.
           MOVE LTL-LIEN-TITLE-STATE  TO XLTL-LIEN-4-TITLE-STATE.
           MOVE LTL-TITLE-STATUS-CODE TO XLTL-LIEN-4-STATUS-CODE.

           MOVE LTL-TITLE-SEND-FLAG TO XLTL-TITLE-5-SEND-FLAG.
           MOVE LTL-TITLE-NAME      TO XLTL-TITLE-5-NAME.
           MOVE LTL-TITLE-ADR       TO XLTL-TITLE-5-ADR.
           MOVE LTL-TITLE-ADR2      TO XLTL-TITLE-5-ADR2.
           MOVE LTL-TITLE-CITY      TO XLTL-TITLE-5-CITY.
           MOVE LTL-TITLE-STATE     TO XLTL-TITLE-5-STATE.
           MOVE LTL-TITLE-ZIP       TO XLTL-TITLE-5-ZIP.
      * XLTL-TITLE-5-DATE IS 9(6), PROBABLY A MISTAKE FROM A15
           MOVE LTL-TITLE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO XLTL-TITLE-5-DATE.

           MOVE LTL-TITLE-SENT-BY TO XLTL-TITLE-5-SENT-BY.

           MOVE LTL-SEND-ASAP TO XLTL-SEND-ASAP.

           MOVE LTL-TITLE-COMMENT1 TO XLTL-TITLE-6-COMMENT1.
           MOVE LTL-TITLE-COMMENT2 TO XLTL-TITLE-6-COMMENT2.
           MOVE LTL-RELEASE-TYPE   TO XLTL-TITLE-6-RELEASE-TYPE.
           MOVE LTL-CARRIER-TYPE   TO XLTL-CARRIER-TYPE.
           MOVE LTL-CARRIER-ACCTNO   TO XLTL-CARRIER-ACCTNO.

           MOVE LN-OWNBR         TO WS-OWNBR.
           MOVE LN-ACCTNO        TO WS-NUMBER.
           MOVE WS-BRACCTNO      TO XLTL-BRACCTNO.
           MOVE BR-REGION-NUMBER TO XLTL-REGION-NUMBER.

      *    MOVE LTL-ACCTNO       TO XLTL-ACCTNO.
      *CS TO MATCH A15 ACCOUNT NUMBER
           MOVE LTL-ACCTNO           TO WS-A15-NUMBER.
           MOVE 01                   TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO        TO XLTL-ACCTNO.
           MOVE "L"              TO XLTL-CODE.
           MOVE 0                TO XLTL-SEQNO XLTL-TYPE.

           MOVE X"0D" TO XLTL-CR.
           MOVE X"0A" TO XLTL-LF.

           ADD 1 TO XLTL-KEY.
           PERFORM WRITE-XLTL-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO LTL-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

       EXIT-EXTRACT-LIENHOLDER.
           EXIT.

      ****************************************************************
       EXTRACT-DEBITCARD-TRAILER SECTION.
      ****************************************************************
           MOVE SPACES TO XLTD-REC.

      *    MOVE LN-OWNBR  TO LTD-BRNO.
      *    MOVE LN-ACCTNO TO LTD-ACCTNO.
      *    MOVE 1         TO LTD-SEQNO.
      *    PERFORM READ-LTD1-FILE.
      *    IF IO-BAD
           IF LTD-ACCTNO = 0
              GO TO EXIT-EXTRACT-DEBITCARD.
      
           MOVE LN-OWNBR         TO WS-OWNBR.
           MOVE LN-ACCTNO        TO WS-NUMBER.
           MOVE WS-BRACCTNO      TO XLTD-BRACCTNO.

      *** TO MAKE REGION NUMBER MATCH A15 FILE FOR SPACES, *** 
      *** THE FOLLOWING "IF" STATEMENT IS PERFORMED        ***
           IF BR-REGION-NUMBER = 00
              MOVE SPACES TO XLTD-REGION-NUMBER
           ELSE
              MOVE BR-REGION-NUMBER TO XLTD-REGION-NUMBER.   

      *    MOVE BR-REGION-NUMBER TO XLTD-REGION-NUMBER.

      *    MOVE LTD-ACCTNO        TO XLTD-ACCTNO.
      *CS TO MATCH A15 ACCOUNT NUMBER
           MOVE LTD-ACCTNO           TO WS-A15-NUMBER.
           MOVE 01                   TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO        TO XLTD-ACCTNO.
           MOVE "D"                  TO XLTD-CODE.
           MOVE LTD-SEQNO            TO XLTD-SEQNO.

      * DUE TO A BUG IN A15, THE XLTD-TYPE WAS COMMENTED OUT
      * SO SPACES ARE IN THIS FIELD, REPLACING ZEROES. 
      *    MOVE 0                    TO XLTD-TYPE.

           MOVE LTD-DEBIT-CARD-NUMBER  TO XLTD-DEBIT-CARD-NUMBER.
           MOVE LTD-DEBIT-CARD-AMT     TO XLTD-DEBIT-CARD-AMT.
           MOVE LTD-LOANDATE           TO XLTD-LOANDATE.
           MOVE LTD-VOID-DATE          TO XLTD-VOID-DATE.

           MOVE LTD-CLEARED-DATE       TO XLTD-CLEARED-DATE.

           MOVE LTD-CLEARED-AMT        TO XLTD-CLEARED-AMT.

           MOVE X"0D" TO XLTD-CR.
           MOVE X"0A" TO XLTD-LF.
              
           ADD 1 TO XLTD-KEY.
           PERFORM WRITE-XLTD-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO LTD-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

       EXIT-EXTRACT-DEBITCARD.
           EXIT.

      ********************************************************
       EXTRACT-BANKRUPT-TRAILER SECTION.
      ******************************************************************

           MOVE SPACES TO XLTB-REC.

      *    MOVE LN-OWNBR  TO LTB-BRNO.
      *    MOVE LN-ACCTNO TO LTB-ACCTNO.
      *    MOVE 1         TO LTB-SEQNO.
      *    PERFORM READ-LTB1-FILE.
      *    IF IO-BAD
           IF LTB-ACCTNO = 0
              GO TO EXIT-EXTRACT-BANKRUPT.

           MOVE LTB-FILEDATE    TO XLTB-FILEDATE.
           MOVE LTB-BDDATE      TO XLTB-BDDATE.

           MOVE LTB-CASE-NO     TO XLTB-CASE-NO.
           MOVE LTB-CHAPTER     TO XLTB-CHAPTER.
           MOVE LTB-MISC1       TO XLTB-MISC1.
           MOVE LTB-MISC2       TO XLTB-MISC2.
           MOVE LTB-MISC3       TO XLTB-MISC3.

           MOVE LTB-WHO-FILED   TO XLTB-WHO-FILED.
           MOVE LTB-ENDORSER-FILED TO XLTB-ENDORSER-FILED.
           MOVE LTB-CLAIM-AMT   TO XLTB-CLAIM-AMT.
           MOVE LTB-ACTIVE-INACTIVE TO XLTB-ACTIVE-INACTIVE.
           MOVE LTB-INT-LC-DUE  TO XLTB-INT-LC-DUE.
           MOVE LTB-OTH-DUE     TO XLTB-OTH-DUE.
           MOVE LTB-TDUEAMT     TO XLTB-TDUEAMT.

           MOVE LTB-NAME(1)        TO XLTB-ATTORNEY-NAME.
           MOVE LTB-ADR1(1)        TO XLTB-ATTORNEY-ADR1.
           MOVE LTB-ADR2(1)        TO XLTB-ATTORNEY-ADR2.
           MOVE LTB-CITY(1)        TO XLTB-ATTORNEY-CITY.
           MOVE LTB-STATE(1)       TO XLTB-ATTORNEY-STATE.
           MOVE LTB-ZIP(1)         TO XLTB-ATTORNEY-ZIP.
           MOVE LTB-PHONE(1)       TO XLTB-ATTORNEY-PHONE.

           MOVE LTB-NAME(2)        TO XLTB-CLERK-NAME.
           MOVE LTB-ADR1(2)        TO XLTB-CLERK-ADR1.
           MOVE LTB-ADR2(2)        TO XLTB-CLERK-ADR2.
           MOVE LTB-CITY(2)        TO XLTB-CLERK-CITY.
           MOVE LTB-STATE(2)       TO XLTB-CLERK-STATE.
           MOVE LTB-ZIP(2)         TO XLTB-CLERK-ZIP.
           MOVE LTB-PHONE(2)       TO XLTB-CLERK-PHONE.

           MOVE LTB-NAME(3)        TO XLTB-TRUSTEE-NAME.
           MOVE LTB-ADR1(3)        TO XLTB-TRUSTEE-ADR1.
           MOVE LTB-ADR2(3)        TO XLTB-TRUSTEE-ADR2.
           MOVE LTB-CITY(3)        TO XLTB-TRUSTEE-CITY.
           MOVE LTB-STATE(3)       TO XLTB-TRUSTEE-STATE.
           MOVE LTB-ZIP(3)         TO XLTB-TRUSTEE-ZIP.
           MOVE LTB-PHONE(3)       TO XLTB-TRUSTEE-PHONE.

           MOVE LTB-REAFF-DATE     TO XLTB-REAFF-DATE.
           MOVE LTB-REAFF-SYSDATE  TO XLTB-REAFF-SYSDATE.

           MOVE LTB-REAFF-BALANCE     TO XLTB-REAFF-BALANCE.
           MOVE LTB-REAFF-PAYAMT      TO XLTB-REAFF-PAYAMT.
           MOVE LTB-REAFF-NO-PAYS     TO XLTB-REAFF-NO-PAYS.
           MOVE LTB-INTRATE           TO XLTB-INTRATE.

           MOVE LTB-RESCIND-DATE      TO XLTB-RESCIND-DATE.

           MOVE LTB-PARTIAL-FULL      TO XLTB-PARTIAL-FULL.
           MOVE LTB-DISCHARGE-DATE    TO XLTB-DISCHARGE-DATE.

           MOVE LTB-DISCHARGE-SYSDATE TO XLTB-DISCHARGE-SYSDATE.

           MOVE LN-OWNBR         TO WS-OWNBR.
           MOVE LN-ACCTNO        TO WS-NUMBER.
           MOVE WS-BRACCTNO      TO XLTB-BRACCTNO.

      *** TO MAKE REGION NUMBER MATCH A15 FILE FOR SPACES, *** 
      *** THE FOLLOWING "IF" STATEMENT IS PERFORMED        ***
           IF BR-REGION-NUMBER = 00
              MOVE SPACES TO XLTB-REGION-NUMBER
           ELSE
              MOVE BR-REGION-NUMBER TO XLTB-REGION-NUMBER.   

      *    MOVE BR-REGION-NUMBER TO XLTB-REGION-NUMBER.

      *    MOVE LTB-ACCTNO       TO XLTB-ACCTNO.
      *CS TO MATCH A15 ACCOUNT NUMBER
           MOVE LTB-ACCTNO           TO WS-A15-NUMBER.
           MOVE 01                   TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO        TO XLTB-ACCTNO.
           MOVE "B"                  TO XLTB-CODE.
           MOVE 0                    TO XLTB-SEQNO XLTB-TYPE.

           MOVE X"0D" TO XLTB-CR.
           MOVE X"0A" TO XLTB-LF.

           ADD 1 TO XLTB-KEY.
           PERFORM WRITE-XLTB-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO LTB-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

       EXIT-EXTRACT-BANKRUPT.
           EXIT.

      ******************************************************************
       EXTRACT-COLLATERAL-TRAILER SECTION.
      ******************************************************************

           MOVE SPACES TO XLTC-REC.

      *    MOVE LN-OWNBR  TO LTC-BRNO.
      *    MOVE LN-ACCTNO TO LTC-ACCTNO.

      *    MOVE 1         TO LTC-SEQNO.

      *    PERFORM READ-LTC1-FILE.
      *    IF IO-BAD
           IF LTC-ACCTNO = 0
              GO TO EXIT-EXTRACT-COLLATERAL.


           MOVE LTC-DESC(1)         TO XLTC-DESCRIPTION1.
           MOVE LTC-MAKE(1)         TO XLTC-MAKE1.
           MOVE LTC-MODEL(1)        TO XLTC-MODEL1.
           MOVE LTC-YEAR(1)         TO XLTC-YEAR1.
           MOVE LTC-STYLE(1)        TO XLTC-STYLE1.
           MOVE LTC-CLASS(1)        TO XLTC-CLASS1.
           MOVE LTC-VIN(1)          TO XLTC-ID1.
           MOVE LTC-TITLE-NO(1)     TO XLTC-TITLE-NO1.
           MOVE LTC-MILEAGE(1)      TO XLTC-MILEAGE1.
           INSPECT XLTC-MILEAGE1 REPLACING LEADING "+" BY " ".
           MOVE LTC-VALUE(1)        TO XLTC-VALUE1.
           INSPECT XLTC-VALUE1 REPLACING LEADING "+" BY " ".
           MOVE LTC-SELLPR(1)       TO XLTC-SELLPR1.
           INSPECT XLTC-SELLPR1 REPLACING LEADING "+" BY " ".
           MOVE LTC-MISC-AMT(1)     TO XLTC-MISC-AMT1.
           INSPECT XLTC-MISC-AMT1 REPLACING LEADING "+" BY " ".

           MOVE LTC-DESC(2)         TO XLTC-DESCRIPTION2.
           MOVE LTC-MAKE(2)         TO XLTC-MAKE2.
           MOVE LTC-MODEL(2)        TO XLTC-MODEL2.
           MOVE LTC-YEAR(2)         TO XLTC-YEAR2.
           MOVE LTC-STYLE(2)        TO XLTC-STYLE2.
           MOVE LTC-CLASS(2)        TO XLTC-CLASS2.
           MOVE LTC-VIN(2)          TO XLTC-ID2.
           MOVE LTC-TITLE-NO(2)     TO XLTC-TITLE-NO2.
           MOVE LTC-MILEAGE(2)      TO XLTC-MILEAGE2.
           INSPECT XLTC-MILEAGE2 REPLACING LEADING "+" BY " ".
           MOVE LTC-VALUE(2)        TO XLTC-VALUE2.
           INSPECT XLTC-VALUE2 REPLACING LEADING "+" BY " ".
           MOVE LTC-SELLPR(2)       TO XLTC-SELLPR2.
           INSPECT XLTC-SELLPR2 REPLACING LEADING "+" BY " ".
           MOVE LTC-MISC-AMT(2)     TO XLTC-MISC-AMT2.
           INSPECT XLTC-MISC-AMT2 REPLACING LEADING "+" BY " ".

           MOVE LTC-HHGS(1)         TO XLTC-GOODS1.

           MOVE LTC-HHGS(2)         TO XLTC-GOODS2.

           MOVE LN-OWNBR         TO WS-OWNBR.
           MOVE LN-ACCTNO        TO WS-NUMBER.
           MOVE WS-BRACCTNO      TO XLTC-BRACCTNO.

      *** TO MAKE REGION NUMBER MATCH A15 FILE FOR SPACES, *** 
      *** THE FOLLOWING "IF" STATEMENT IS PERFORMED        ***
           IF BR-REGION-NUMBER = 00
              MOVE SPACES TO XLTC-REGION-NUMBER
           ELSE
              MOVE BR-REGION-NUMBER TO XLTC-REGION-NUMBER.

      *    MOVE BR-REGION-NUMBER TO XLTC-REGION-NUMBER.

      *    MOVE LTC-ACCTNO       TO XLTC-ACCTNO.
      *CS TO MATCH A15 ACCOUNT NUMBER
           MOVE LTC-ACCTNO           TO WS-A15-NUMBER.
           MOVE 01                   TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO        TO XLTC-ACCTNO.
           MOVE 0                TO XLTC-SEQNO XLTC-TYPE.
           MOVE "C"              TO XLTC-CODE.

           MOVE X"0D" TO XLTC-CR.
           MOVE X"0A" TO XLTC-LF.

           ADD 1 TO XLTC-KEY.
           PERFORM WRITE-XLTC-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO LTC-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

       EXIT-EXTRACT-COLLATERAL.
           EXIT.

      ******************************************************************
       EXTRACT-ADDON-TRAILER SECTION.
      ******************************************************************
           MOVE LTI-PATH-OWNBR  TO LTI-BRNO
                                   QLTI1-WBEG-BRNO
                                   QLTI1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LTI-ACCTNO
                                   QLTI1-WBEG-ACCTNO
                                   QLTI1-WEND-ACCTNO.
           MOVE 0               TO LTI-SEQNO
                                   QLTI1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLTI1-WEND-SEQNO.

           PERFORM START-LTI1-FILE.
       NEXT-LTI.
           PERFORM READ-LTI1-FILE-NEXT.
           IF IO-BAD OR (LTI-BRNO NOT = LN-OWNBR)
              GO TO EXIT-EXTRACT-ADDONS.

           IF LTI-ACCTNO NOT = LN-ACCTNO
              GO TO EXIT-EXTRACT-ADDONS.

           MOVE SPACES TO XLTI-REC.

           MOVE LN-OWNBR            TO WS-OWNBR.
           MOVE LN-ACCTNO           TO WS-NUMBER.
           MOVE WS-BRACCTNO         TO XLTI-BRACCTNO.
           MOVE BR-REGION-NUMBER    TO XLTI-REGION-NUMBER.

      *    MOVE LTI-ACCTNO           TO XLTI-ACCTNO.
      *CS TO MATCH A15 ACCOUNT NUMBER
           MOVE LTI-ACCTNO           TO WS-A15-NUMBER.
           MOVE 01                   TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO        TO XLTI-ACCTNO.
      *CS NEEDED TO MATCH A15 OUTPUT
           MOVE "I"                  TO XLTI-CODE.
           MOVE LTI-SEQNO            TO XLTI-SEQNO.
           MOVE LTI-INS-TYPE         TO XLTI-TYPE.

           MOVE LTI-ORIG-1STPYDATE   TO XLTI-ORIG-1STPAY.

           MOVE LTI-PRIOR-NODEF      TO XLTI-PRIOR-NODEF.
           INSPECT XLTI-PRIOR-NODEF REPLACING LEADING "+" BY " ".
           MOVE LTI-INSFEECD(1)      TO XLTI-INSFEECD1.
           MOVE LTI-INSFEE(1)        TO XLTI-INSFEE1.
           MOVE LTI-INSFEECD(2)      TO XLTI-INSFEECD2.
           MOVE LTI-INSFEE(2)        TO XLTI-INSFEE2.
           MOVE LTI-TRCD             TO XLTI-TRCD.
           MOVE LTI-INSCOMP          TO XLTI-INSCOMP.
           MOVE LTI-INSOURS          TO XLTI-INSOURS.

           MOVE LTI-INSEFF-DATE      TO XLTI-INSEFF.
           MOVE LTI-INSEXP-DATE      TO XLTI-INSEXP.

           MOVE LTI-INSPREM          TO XLTI-INSPREM.
           INSPECT XLTI-INSPREM REPLACING LEADING "+" BY " ".
           MOVE LTI-INSCOVR          TO XLTI-INSCOVR.
           INSPECT XLTI-INSCOVR REPLACING LEADING "+" BY " ".
           MOVE LTI-INSCOMM          TO XLTI-INSCOMM.
           INSPECT XLTI-INSCOMM REPLACING LEADING "+" BY " ".
           MOVE LTI-ANTICTERM        TO XLTI-ANTICTERM.
           MOVE LTI-ANTICERN(1)      TO XLTI-ANTICERN1.
           INSPECT XLTI-ANTICERN1 REPLACING LEADING "+" BY " ".
           MOVE LTI-ANTICADJ(1)      TO XLTI-ANTICADJ1.
           INSPECT XLTI-ANTICADJ1 REPLACING LEADING "+" BY " ".
           MOVE LTI-UFERN(1)         TO XLTI-UFERN1.
           INSPECT XLTI-UFERN1 REPLACING LEADING "+" BY " ".
           MOVE LTI-ACCERN(1)        TO XLTI-ACCERN1.
           INSPECT XLTI-ACCERN1 REPLACING LEADING "+" BY " ".
           MOVE LTI-ANTICERN(2)      TO XLTI-ANTICERN2.
           INSPECT XLTI-ANTICERN2 REPLACING LEADING "+" BY " ".
           MOVE LTI-ANTICADJ(2)      TO XLTI-ANTICADJ2.
           INSPECT XLTI-ANTICADJ2 REPLACING LEADING "+" BY " ".
           MOVE LTI-UFERN(2)         TO XLTI-UFERN2.
           INSPECT XLTI-UFERN2 REPLACING LEADING "+" BY " ".
           MOVE LTI-ACCERN(2)        TO XLTI-ACCERN2.
           INSPECT XLTI-ACCERN2 REPLACING LEADING "+" BY " ".
           MOVE LTI-ANTICERN(3)      TO XLTI-ANTICERN3.
           INSPECT XLTI-ANTICERN3 REPLACING LEADING "+" BY " ".
           MOVE LTI-ANTICADJ(3)      TO XLTI-ANTICADJ3.
           INSPECT XLTI-ANTICADJ3 REPLACING LEADING "+" BY " ".
           MOVE LTI-UFERN(3)         TO XLTI-UFERN3.
           INSPECT XLTI-UFERN3 REPLACING LEADING "+" BY " ".
           MOVE LTI-ACCERN(3)        TO XLTI-ACCERN3.
           INSPECT XLTI-ACCERN3 REPLACING LEADING "+" BY " ".
           MOVE LTI-INT-REBATE       TO XLTI-INT-REBATE.
           MOVE LTI-INS-PAYMNT-INCR  TO XLTI-INS-PAYMNT-INCR.
           INSPECT XLTI-INS-PAYMNT-INCR REPLACING LEADING "+" BY " ".
           MOVE LTI-INS-TOT-ADDON    TO XLTI-INS-TOT-ADDON.
           INSPECT XLTI-INS-TOT-ADDON REPLACING LEADING "+" BY " ".
           MOVE LTI-INT-LAST-EARNED  TO XLTI-INT-LAST-EARNED.
           INSPECT XLTI-INT-LAST-EARNED REPLACING LEADING "+" BY " ".
           MOVE LTI-INS-REBATE       TO XLTI-INS-REBATE.
           MOVE LTI-RB-WORKER        TO XLTI-RB-WORKER.
           INSPECT XLTI-RB-WORKER REPLACING LEADING "+" BY " ".

           MOVE LTI-ANTIC-START-DATE TO XLTI-ANTIC-START-DATE.

           MOVE X"0D" TO XLTI-CR.
           MOVE X"0A" TO XLTI-LF.

           ADD 1 TO XLTI-KEY.
           PERFORM WRITE-XLTI-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO LTI-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO NEXT-LTI.

       EXIT-EXTRACT-ADDONS.
           EXIT.

      ******************************************************************
       EXTRACT-PAYMENT-SCHEDULE-TRAILER SECTION.
      ******************************************************************
           MOVE SPACES TO XLTP-REC.

      *    MOVE LN-OWNBR  TO LTP-BRNO.
      *    MOVE LN-ACCTNO TO LTP-ACCTNO.
      *    MOVE 1         TO LTP-SEQNO.

      *    PERFORM READ-LTP1-FILE.
      *    IF IO-BAD
           IF LTP-ACCTNO = 0
              GO TO EXIT-EXTRACT-PAYMENT-SCHEDULE.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 17
              MOVE LTP-SCHLD-TERM(SUB)   TO XLTP-SCHLD-TERM(SUB)
              MOVE LTP-SCHLD-PAYMNT(SUB) TO XLTP-SCHLD-PAYMNT(SUB)
           END-PERFORM.

           MOVE LN-OWNBR         TO WS-OWNBR.
           MOVE LN-ACCTNO        TO WS-NUMBER.
           MOVE WS-BRACCTNO      TO XLTP-BRACCTNO.
           MOVE BR-REGION-NUMBER TO XLTP-REGION-NUMBER.

      *    MOVE LTP-ACCTNO        TO XLTP-ACCTNO.
      *CS TO MATCH A15 ACCOUNT NUMBER
           MOVE LTP-ACCTNO           TO WS-A15-NUMBER.
           MOVE 01                   TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO        TO XLTP-ACCTNO.
           MOVE "P"                  TO XLTP-CODE.
           MOVE 0                TO XLTP-SEQNO XLTP-TYPE.

           MOVE X"0D" TO XLTP-CR.
           MOVE X"0A" TO XLTP-LF.

           ADD 1 TO XLTP-KEY.
           PERFORM WRITE-XLTP-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO LTP-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

       EXIT-EXTRACT-PAYMENT-SCHEDULE.
           EXIT.


      ******************************************************************
       EXTRACT-JUDGMENT-TRAILER SECTION.
      ******************************************************************
           MOVE SPACES TO XLSJ-REC.

      *    MOVE LN-OWNBR  TO LSJ-BRNO.
      *    MOVE LN-ACCTNO TO LSJ-ACCTNO.
      *    MOVE 1         TO LSJ-SEQNO.
      *    PERFORM READ-LSJ1-FILE.
      *    IF IO-BAD
           IF LSJ-ACCTNO = 0
              GO TO EXIT-EXTRACT-JUDGMENT.

           MOVE LN-OWNBR               TO WS-OWNBR.
           MOVE LN-ACCTNO              TO WS-NUMBER.
           MOVE WS-BRACCTNO            TO XLS-J-BRACCTNO.
      *    MOVE LN-ACCTNO              TO XLS-J-ACCTNO.
      *CS TO MATCH A15 ACCOUNT NUMBER
           MOVE LN-ACCTNO            TO WS-A15-NUMBER.
           MOVE 01                   TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO        TO XLS-J-ACCTNO.

           MOVE "J"                    TO XLS-J-CODE.
           MOVE LSJ-SEQNO              TO XLS-J-SEQNO.

           MOVE LSJ-FILEDATE           TO XLS-J-FILEDATE.

           MOVE LSJ-CASE-NO     TO XLS-J-CASE-NO.
           MOVE LSJ-AMT         TO XLS-J-AMT.
           MOVE LSJ-COUNTY      TO XLS-J-COUNTY.
           MOVE LSJ-CLERK-NAME  TO XLS-J-CLERK-NAME.
           MOVE LSJ-CLERK-ADDR  TO XLS-J-CLERK-ADDR.
           MOVE LSJ-CLERK-CITY  TO XLS-J-CLERK-CITY.
           MOVE LSJ-CLERK-STATE TO XLS-J-CLERK-STATE.
           MOVE LSJ-CLERK-ZIP   TO XLS-J-CLERK-ZIP.
           MOVE LSJ-CLERK-PHONE TO XLS-J-CLERK-PHONE.

           MOVE LSJ-SHERIFF-FEE TO XLS-J-SHERIFF-FEE.
           MOVE LSJ-COURT-FEE   TO XLS-J-COURT-FEE.
           MOVE LSJ-ATTY-FEE    TO XLS-J-ATTY-FEE.

           MOVE LSJ-DISMISS-DATE TO XLS-J-DISMISS-DATE.

           MOVE LSJ-GARN-INFO     TO XLS-J-GARN-INFO.
           MOVE LSJ-EMPLOY-NAME   TO XLS-J-EMPLOY-NAME.
           MOVE LSJ-EMPLOY-ADDR   TO XLS-J-EMPLOY-ADDR.
           MOVE LSJ-EMPLOY-CITY   TO XLS-J-EMPLOY-CITY.
           MOVE LSJ-EMPLOY-STATE  TO XLS-J-EMPLOY-STATE.
           MOVE LSJ-EMPLOY-ZIP    TO XLS-J-EMPLOY-ZIP.
           MOVE LSJ-GARN-DATE     TO XLS-J-GARN-DATE.
           MOVE LSJ-JD-FILED-ON   TO XLS-J-JD-FILED-ON.
           MOVE LSJ-GARN-FILED-ON TO XLS-J-GARN-FILED-ON.

           MOVE LSJ-ATTY-NAME TO XLS-J-ATTY-NAME.
           MOVE LSJ-ATTY-ADDR TO XLS-J-ATTY-ADDR.
           MOVE LSJ-ATTY-CITY TO XLS-J-ATTY-CITY.
           MOVE LSJ-ATTY-STATE TO XLS-J-ATTY-STATE.
           MOVE LSJ-ATTY-ZIP   TO XLS-J-ATTY-ZIP.
           MOVE LSJ-ATTY-PHONE TO XLS-J-ATTY-PHONE.

           MOVE X"0D"                  TO XLS-J-CR.
           MOVE X"0A"                  TO XLS-J-LF.
              
           ADD 1 TO XLSJ-KEY.
           PERFORM WRITE-XLSJ-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO LSJ-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

       EXIT-EXTRACT-JUDGMENT.
           EXIT.

      ******************************************************************
       EXTRACT-F9MISC-TRAILER SECTION.
      ******************************************************************
           MOVE SPACES TO XLSM-REC.

      *    MOVE LN-OWNBR  TO LSM-BRNO.
      *    MOVE LN-ACCTNO TO LSM-ACCTNO.
      *    MOVE 1         TO LSM-SEQNO.
      *    PERFORM READ-LSM1-FILE.
      *    IF IO-BAD
           IF LSM-ACCTNO = 0
              GO TO EXIT-EXTRACT-F9MISC.

           MOVE LN-OWNBR               TO WS-OWNBR.
           MOVE LN-ACCTNO              TO WS-NUMBER.
           MOVE WS-BRACCTNO            TO XLS-M-BRACCTNO.
      *    MOVE LN-ACCTNO              TO XLS-M-ACCTNO.
      *CS TO MATCH A15 ACCOUNT NUMBER
           MOVE LN-ACCTNO              TO WS-A15-NUMBER.
           MOVE 01                     TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO          TO XLS-M-ACCTNO.
           MOVE "M"                    TO XLS-M-CODE.
           MOVE LSM-SEQNO              TO XLS-M-SEQNO.

           MOVE LSM-MISC-AREA          TO XLS-MISC-AREA.

           MOVE LSM-TRANS-FROM-BRNO    TO XLS-M-TRANS-FROM-BRNO.
           MOVE LSM-TRANS-FROM-ACCTNO  TO XLS-M-TRANS-FROM-ACCTNO.
           MOVE LSM-TRANS-FROM-DATE    TO XLS-M-TRANS-FROM-DATE.
           MOVE LSM-MISPAY-90-DATE     TO XLS-M-MISPAY-90-DATE.

           MOVE LSM-SECOND-BEN-NAME   TO XLS-M-SECOND-BEN-NAME.
           MOVE LSM-SECOND-BEN-ADR    TO XLS-M-SECOND-BEN-ADR.
           MOVE LSM-SECOND-BEN-CITY   TO XLS-M-SECOND-BEN-CITY.
           MOVE LSM-SECOND-BEN-STATE  TO XLS-M-SECOND-BEN-STATE.
           MOVE LSM-SECOND-BEN-ZIP    TO XLS-M-SECOND-BEN-ZIP.
           MOVE LSM-SECOND-BEN-PHONE  TO XLS-M-SECOND-BEN-PHONE.
           MOVE LSM-SECOND-BEN-RELATION TO XLS-M-SECOND-BEN-RELATION.

           MOVE X"0D"                  TO XLS-M-CR.
           MOVE X"0A"                  TO XLS-M-LF.
              
           ADD 1 TO XLSM-KEY.
           PERFORM WRITE-XLSM-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO LSM-RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

       EXIT-EXTRACT-F9MISC.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE LTEXTR-QUEUE-POS TO Q-POS.
           MOVE LTEXTR-COPIES-POS TO C-POS.
           MOVE LTEXTR-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           CALL "C$TOLOWER" USING XLS-DIRECTORY, VALUE 13.
           MOVE XLS-DIRECTORY TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "EXTRACTION DIRECTORY DOES NOT EXIST!" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE AGE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE TO H-AGEDATE.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      *     PERFORM OPEN-SP1-FILE.
           PERFORM ARRAYSP-LOAD.

           IF EXT-ROUTE-BUF = "00"
              MOVE "EXTRACTION IN PROGRESS........." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

      * ALWAYS CREATE COLLATERAL
           
           CALL "C$TOLOWER" USING XLTC-DIRECTORY, VALUE 13.
           PERFORM OPEN-XLTC-FILE-OUTPUT.
           PERFORM CLOSE-XLTC-FILE.
           PERFORM OPEN-XLTC-FILE.

      * CREATE ONLY THE COLLATERAL FILE Y/N
           IF COLL NOT = "Y"
              CALL "C$TOLOWER" USING XLS-DIRECTORY, VALUE 13
              PERFORM OPEN-XLS-FILE-OUTPUT
              PERFORM CLOSE-XLS-FILE
              PERFORM OPEN-XLS-FILE

              CALL "C$TOLOWER" USING XLSD-DIRECTORY, VALUE 13
              PERFORM OPEN-XLSD-FILE-OUTPUT
              PERFORM CLOSE-XLSD-FILE
              PERFORM OPEN-XLSD-FILE

              CALL "C$TOLOWER" USING XLSR-DIRECTORY, VALUE 13
              PERFORM OPEN-XLSR-FILE-OUTPUT
              PERFORM CLOSE-XLSR-FILE
              PERFORM OPEN-XLSR-FILE

              CALL "C$TOLOWER" USING XLSM-DIRECTORY, VALUE 13
              PERFORM OPEN-XLSM-FILE-OUTPUT
              PERFORM CLOSE-XLSM-FILE
              PERFORM OPEN-XLSM-FILE

              CALL "C$TOLOWER" USING XLSJ-DIRECTORY, VALUE 13
              PERFORM OPEN-XLSJ-FILE-OUTPUT
              PERFORM CLOSE-XLSJ-FILE
              PERFORM OPEN-XLSJ-FILE

              CALL "C$TOLOWER" USING XLSF-DIRECTORY, VALUE 13
              PERFORM OPEN-XLSF-FILE-OUTPUT
              PERFORM CLOSE-XLSF-FILE
              PERFORM OPEN-XLSF-FILE

              CALL "C$TOLOWER" USING XLSL-DIRECTORY, VALUE 13
              PERFORM OPEN-XLSL-FILE-OUTPUT
              PERFORM CLOSE-XLSL-FILE
              PERFORM OPEN-XLSL-FILE

              CALL "C$TOLOWER" USING XLTA-DIRECTORY, VALUE 13
              PERFORM OPEN-XLTA-FILE-OUTPUT
              PERFORM CLOSE-XLTA-FILE
              PERFORM OPEN-XLTA-FILE

              CALL "C$TOLOWER" USING XLTL-DIRECTORY, VALUE 13
              PERFORM OPEN-XLTL-FILE-OUTPUT
              PERFORM CLOSE-XLTL-FILE
              PERFORM OPEN-XLTL-FILE

              CALL "C$TOLOWER" USING XLTB-DIRECTORY, VALUE 13
              PERFORM OPEN-XLTB-FILE-OUTPUT
              PERFORM CLOSE-XLTB-FILE
              PERFORM OPEN-XLTB-FILE

              CALL "C$TOLOWER" USING XLTI-DIRECTORY, VALUE 13
              PERFORM OPEN-XLTI-FILE-OUTPUT
              PERFORM CLOSE-XLTI-FILE
              PERFORM OPEN-XLTI-FILE

              CALL "C$TOLOWER" USING XLTP-DIRECTORY, VALUE 13
              PERFORM OPEN-XLTP-FILE-OUTPUT
              PERFORM CLOSE-XLTP-FILE
              PERFORM OPEN-XLTP-FILE

              CALL "C$TOLOWER" USING XLTD-DIRECTORY, VALUE 13
              PERFORM OPEN-XLTD-FILE-OUTPUT
              PERFORM CLOSE-XLTD-FILE 
              PERFORM OPEN-XLTD-FILE.
 

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO AGE-DATE.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * DONT GO FURTHER IF NOT A VALID LOGIN
      * NEED USERS BRANCH FOR BR-FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           IF VDUSTATUS = "1U"
              GO TO COLLYN.

       AGEDT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BATCH WILL USE DATE RAN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060AGEDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGEDT.

           MOVE VDU-DATE-YYYYMMDD TO AGE-DATE.

           IF AGE-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000AGEDATE." TO VDU
              MOVE DATE-YYYYMMDD TO AGE-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO AGEDT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S NA000BR1." TO VDU
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
              MOVE "S  A160DISPLAY-USER." TO VDU
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

      *----------------------------------------------------------------
       BR-02.
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

           MOVE "S NA160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
      *----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.
           GO TO PLYN.
      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       PLYN.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PLYN.
           MOVE VDUBUF TO PL.

           IF PL = "I" OR "O"
              GO TO PLTYPEYN.

           IF PL NOT = "S"
              GO TO PLYN.

      * PL = "S"
           MOVE "B" TO PLACTIVE.
           MOVE "S  8000PLDATE." TO VDU.
           MOVE 0 TO PL-FROM-DATE TEST-DATE-PAID-LAST
                                  VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           GO TO ZBALYN.
      *----------------------------------------------------------------
       PLTYPEYN.
           MOVE "E  A010PLTYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PLYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLTYPEYN.
           MOVE VDUBUF TO PLTYPE.
           IF NOT (PLTYPE = "A" OR "P" OR "2" OR "3" OR "4" OR "5")
              GO TO PLTYPEYN.
      *----------------------------------------------------------------
       ACTVYN.
           MOVE "E  A010ACTIVE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PLTYPEYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ACTVYN.
           MOVE VDUBUF TO PLACTIVE.
           IF PLACTIVE NOT = "I" AND NOT = "A" AND NOT = "B"
              GO TO ACTVYN.
      *----------------------------------------------------------------
       ENTER-PL-DATE.
           IF PL = "S"
              MOVE "S  8000PLDATE." TO VDU
              MOVE 0 TO PL-FROM-DATE TEST-DATE-PAID-LAST
                                     VDU-DATE-YYYYMMDD
              PERFORM SCREENIO
              GO TO ZBALYN.

           MOVE "ERNM060PLDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ACTVYN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-PL-DATE.

           MOVE VDU-DATE-YYYYMMDD TO PL-FROM-DATE.
           IF PL-FROM-DATE = 0
              GO TO ENTER-PL-DATE.


      *----------------------------------------------------------------
       ZBALYN.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF PL = "S"
                 GO TO PLYN
              ELSE
                 GO TO ENTER-PL-DATE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ZBALYN.
           MOVE VDUBUF TO ZBAL.
           IF ZBAL NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO ZBALYN.
       ZBAL-DATE.
           IF ZBAL = "S"
              MOVE "S  8000ZDATE." TO VDU
              MOVE 0 TO ZBAL-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO
              GO TO COLLYN.

           MOVE "ERNM060ZDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ZBALYN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ZBAL-DATE.

           MOVE VDU-DATE-YYYYMMDD TO ZBAL-DATE.

           IF ZBAL-DATE = ZERO
              MOVE "S  8000ZDATE." TO VDU
              MOVE EXT-JULIAN-BEG  TO ZBAL-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       COLLYN.
           MOVE "E  A010COLL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ZBALYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO COLLYN.
           MOVE VDUBUF TO COLL.
           IF COLL NOT = "Y" AND NOT = "N"
              GO TO COLLYN.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNA040BR2." TO VDU
              MOVE SPACES TO VDUNUM0
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
      
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "S  A010PL." TO VDU.
           MOVE PL TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010ACTIVE." TO VDU.
           MOVE PLACTIVE TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010PLTYPE." TO VDU.
           MOVE PLTYPE TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  8000PLDATE." TO VDU.
           MOVE PL-FROM-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  8000ZDATE." TO VDU.
           MOVE ZBAL-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      
           MOVE "S  A010COLL." TO VDU.
           MOVE COLL TO VDUBUF.
           PERFORM SCREENIO.

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
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH         :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH            :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH         :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH            :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEGINNING CLASS          :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING CLASS             :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "P & L                    :" TO RANGE-LINE.
           MOVE PL                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "P&L TYPE                 :" TO RANGE-LINE.
           MOVE PLTYPE                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ACTIVE                   :" TO RANGE-LINE.
           MOVE PLACTIVE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "P & L FROM DATE    :" TO RANGE-LINE.
           MOVE PL-FROM-DATE           TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "3+ YEARS LAST DATE :" TO RANGE-LINE.
           MOVE TEST-DATE-PAID-LAST    TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ZERO BALANCES            :" TO RANGE-LINE.
           MOVE ZBAL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ZERO BALANCES FROM :" TO RANGE-LINE.
           MOVE ZBAL-DATE              TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "COLLATERAL ONLY          :" TO RANGE-LINE.
           MOVE COLL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "STARTING TIME            :" TO RANGE-LINE.
           MOVE STARTING-TIME         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING   TIME            :" TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# REPO RECORDS           :" TO RANGE-LINE.
           MOVE LS-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# DEFICIENCY RECORDS     :" TO RANGE-LINE.
           MOVE LSD-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# RESCHDL/SCRA RECORDS   :" TO RANGE-LINE.
           MOVE LSR-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# FOLLOWUP RECORDS       :" TO RANGE-LINE.
           MOVE LSF-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# LEGAL RECORDS          :" TO RANGE-LINE.
           MOVE LSL-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# F9 MISC FIELDS RECORDS :" TO RANGE-LINE.
           MOVE LSM-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# AUTO INS RECORDS       :" TO RANGE-LINE.
           MOVE LTA-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# LIENHOLDER RECORDS     :" TO RANGE-LINE.
           MOVE LTL-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# PMT SCHED RECORDS      :" TO RANGE-LINE.
           MOVE LTP-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# BANKRUPT RECORDS       :" TO RANGE-LINE.
           MOVE LTB-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# COLLATERAL RECORDS     :" TO RANGE-LINE.
           MOVE LTC-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# ADDON INS RECORDS      :" TO RANGE-LINE.
           MOVE LTI-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# JUDGMENT RECORDS :"       TO RANGE-LINE.
           MOVE LSJ-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# DEBIT CARD RECORDS     :" TO RANGE-LINE.
           MOVE LTD-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

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

       END-HEAD-PAGE.
           MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *************************************************************
       CLASS-SCAN SECTION.
      *************************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020CL1." TO VDU
           ELSE
              MOVE "SNNN020CL2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ******************************************************************
      *     C O P Y    L I B R A R Y    S E C T I O N S
      ******************************************************************

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

       COPY "LIBLP/LPCRNO.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPDEF.CPY".
       COPY "LIBLP/DEFPOL.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPNOCL.CPY".
       COPY "LIBLP/LPTDEL.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO LTEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LTEXTR-SYSDATE.
           DISPLAY LTEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LTEXTR-FORM-FIELDS TO WR-BUF.
           MOVE LTEXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE LTEXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE LTEXTR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/LTEXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
      *----------------------------------------------------------------
       CONDITIONAL-PRINT.
           IF DETAIL-LINE NOT = " "
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTLGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTDGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLTAGS_SQL.CPY".
       COPY "LIBLP/LPLSXGS_SQL.CPY".
       COPY "LIBLP/LPLSRGS_SQL.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBLP/LPLSLGS_SQL.CPY".
       COPY "LIBLP/LPLSJGS_SQL.CPY".
       COPY "LIBLP/LPLSFGS_SQL.CPY".
       COPY "LIBLP/LPLSDGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBEX/EXXLSI.CPY".
       COPY "LIBEX/EXXLSDI.CPY".
       COPY "LIBEX/EXXLSRI.CPY".
       COPY "LIBEX/EXXLSMI.CPY".
       COPY "LIBEX/EXXLSJI.CPY".
       COPY "LIBEX/EXXLSFI.CPY".
       COPY "LIBEX/EXXLSLI.CPY".
       COPY "LIBEX/EXXLTAI.CPY".
       COPY "LIBEX/EXXLTLI.CPY".
       COPY "LIBEX/EXXLTBI.CPY".
       COPY "LIBEX/EXXLTCI.CPY".
       COPY "LIBEX/EXXLTDI.CPY".
       COPY "LIBEX/EXXLTII.CPY".
       COPY "LIBEX/EXXLTPI.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLTA1RN.CPY".
       COPY "LIBLP/LPLTB1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTD1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTL1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLSD1RN.CPY".
       COPY "LIBLP/LPLSF1RN.CPY".
       COPY "LIBLP/LPLSJ1RN.CPY".
       COPY "LIBLP/LPLSL1RN.CPY".
       COPY "LIBLP/LPLSM1RN.CPY".
       COPY "LIBLP/LPLSR1RN.CPY".
       COPY "LIBLP/LPLSX1RN.CPY".

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

           IF LN3-LSLT-CONN-OPEN-SW = "N" 
            MOVE "Y"         TO LN3-LSLT-CONN-OPEN-SW
            EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_LSLT_CONNECT 
                     USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
            END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-LSLT-FILE. *> SSP - LN,LSM,LTB,LTC,LTD
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           IF ( QLN3-WEND-ACCTNO = ZEROES    ) OR
              ( QLN3-WEND-ACCTNO NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
              MOVE LN-CLASS             TO QLN3-WBEG-CLASS
              MOVE ALL "9"              TO QLN3-WEND-CLASS
              MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN3-WEND-ACCTNO.

      *  STORED PROCEDURE:
      *
      *  SELECT TEMP.LN_OWNBR,
      *         TEMP.LN_ACCTNO,
      *         TEMP.LN_CLASS,
      *         TEMP.LN_PLDATE,
      *         TEMP.LN_PLCD,
      *         TEMP.LN_PLTYPE,
      *         TEMP.LN_DATE_PAID_LAST,
      *         TEMP.LN_CURBAL,
      *         TEMP.LN_INTBAL,  
      *         TEMP.LN_LCBAL,  
      *         TEMP.LN_OTHBAL,
      *         TEMP.LN_POFFDATE,
      *         TEMP.LN_PAY_SCHLD_FG,
      *         TEMP.LN_ORGST,   
      *         TEMP.LN_SPRCLASS,
      *         TEMP.LN_SUBCLASS,
      *         TEMP.LN_LAWCODE, 
      *         TEMP.LN_MAKERCD1, 
      *         ISNULL(LSMFILE.LSM_BRNO, 0),
      *         ISNULL(LSMFILE.LSM_ACCTNO, 0),
      *         ISNULL(LSMFILE.LSM_SEQNO, 0),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_1,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_2,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_3,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_4,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_5,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_6,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_7,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_8,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_9,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_10,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_11,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_12,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_13,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_14,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_15,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_16,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_17,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_18,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_19,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_20,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_21,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_22,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_23,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_24,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_25,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_26,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_27,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_28,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_29,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_30,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_31,''),
      *         ISNULL(LSMFILE.LSM_MISC_FIELD_32,''),
      *         ISNULL(LSMFILE.LSM_TRANS_FROM_BRNO, 0),
      *         ISNULL(LSMFILE.LSM_TRANS_FROM_ACCTNO, 0),
      *         ISNULL(CAST(LSMFILE.LSM_TRANS_FROM_DATE 
      *                                  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(CAST(LSMFILE.LSM_MISPAY_90_DATE  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(LSMFILE.LSM_SECOND_BEN_NAME,''),
      *         ISNULL(LSMFILE.LSM_SECOND_BEN_ADR,''),
      *         ISNULL(LSMFILE.LSM_SECOND_BEN_CITY,''),
      *         ISNULL(LSMFILE.LSM_SECOND_BEN_ST,''),
      *         ISNULL(LSMFILE.LSM_SECOND_BEN_ZIP,''),
      *         ISNULL(LSMFILE.LSM_SECOND_BEN_PHONE, 0),
      *         ISNULL(LSMFILE.LSM_SECOND_BEN_RELATION,''),
      *         ISNULL(LTBFILE.LTB_BRNO, 0),
      *         ISNULL(LTBFILE.LTB_ACCTNO, 0),
      *         ISNULL(LTBFILE.LTB_SEQNO, 0),
      *         ISNULL(CAST(LTBFILE.LTB_FILEDATE 
      *                                  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(LTBFILE.LTB_CASE_NO,''),
      *         ISNULL(LTBFILE.LTB_CHAPTER, 0),
      *         ISNULL(LTBFILE.LTB_MISC1,''),
      *         ISNULL(LTBFILE.LTB_MISC2,''),
      *         ISNULL(LTBFILE.LTB_MISC3,''),
      *         ISNULL(CAST(LTBFILE.LTB_BDDATE   
      *                                  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(LTBFILE.LTB_WHO_FILED,''),
      *         ISNULL(LTBFILE.LTB_ENDORSER_FILED,''),
      *         ISNULL(LTBFILE.LTB_CLAIM_AMT, 0),
      *         ISNULL(LTBFILE.LTB_ACTIVE_INACTIVE,''),
      *         ISNULL(LTBFILE.LTB_INT_LC_DUE, 0),
      *         ISNULL(LTBFILE.LTB_OTH_DUE, 0),
      *         ISNULL(LTBFILE.LTB_TDUEAMT, 0),
      *         ISNULL(LTBFILE.LTB_NAME_1,''),
      *         ISNULL(LTBFILE.LTB_NAME_2,''),
      *         ISNULL(LTBFILE.LTB_NAME_3,''),
      *         ISNULL(LTBFILE.LTB_ADR1_1,''),
      *         ISNULL(LTBFILE.LTB_ADR1_2,''),
      *         ISNULL(LTBFILE.LTB_ADR1_3,''),
      *         ISNULL(LTBFILE.LTB_ADR2_1,''),
      *         ISNULL(LTBFILE.LTB_ADR2_2,''),
      *         ISNULL(LTBFILE.LTB_ADR2_3,''),
      *         ISNULL(LTBFILE.LTB_CITY_1,''),
      *         ISNULL(LTBFILE.LTB_CITY_2,''),
      *         ISNULL(LTBFILE.LTB_CITY_3,''),
      *         ISNULL(LTBFILE.LTB_STATE_1,''),
      *         ISNULL(LTBFILE.LTB_STATE_2,''),
      *         ISNULL(LTBFILE.LTB_STATE_3,''),
      *         ISNULL(LTBFILE.LTB_ZIP_1,''),
      *         ISNULL(LTBFILE.LTB_ZIP_2,''),
      *         ISNULL(LTBFILE.LTB_ZIP_3,''),
      *         ISNULL(LTBFILE.LTB_PHONE_1, 0),
      *         ISNULL(LTBFILE.LTB_PHONE_2, 0),
      *         ISNULL(LTBFILE.LTB_PHONE_3, 0),
      *         ISNULL(CAST(LTBFILE.LTB_REAFF_DATE     
      *                                 AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(CAST(LTBFILE.LTB_REAFF_SYSDATE  
      *                                 AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(LTBFILE.LTB_REAFF_BALANCE, 0),
      *         ISNULL(LTBFILE.LTB_REAFF_PAYAMT, 0),
      *         ISNULL(LTBFILE.LTB_REAFF_NO_PAYS, 0),
      *         ISNULL(LTBFILE.LTB_INTRATE, 0),
      *         ISNULL(CAST(LTBFILE.LTB_RESCIND_DATE   
      *                                  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(LTBFILE.LTB_PARTIAL_FULL,''),
      *         ISNULL(CAST(LTBFILE.LTB_DISCHARGE_DATE 
      *                                  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(CAST(LTBFILE.LTB_DISCHARGE_SYSDATE 
      *                                  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(LTCFILE.LTC_BRNO, 0),
      *         ISNULL(LTCFILE.LTC_ACCTNO, 0),
      *         ISNULL(LTCFILE.LTC_SEQNO, 0),
      *         ISNULL(LTCFILE.LTC_DESC_1,''),
      *         ISNULL(LTCFILE.LTC_MAKE_1,''),
      *         ISNULL(LTCFILE.LTC_MODEL_1,''),
      *         ISNULL(LTCFILE.LTC_YEAR_1,''),
      *         ISNULL(LTCFILE.LTC_STYLE_1,''),
      *         ISNULL(LTCFILE.LTC_CLASS_1,''),
      *         ISNULL(LTCFILE.LTC_VIN_1,''),
      *         ISNULL(LTCFILE.LTC_TITLE_NO_1,''),
      *         ISNULL(LTCFILE.LTC_MILEAGE_1, 0),
      *         ISNULL(LTCFILE.LTC_VALUE_1, 0),
      *         ISNULL(LTCFILE.LTC_SELLPR_1, 0),
      *         ISNULL(LTCFILE.LTC_MISC_AMT_1, 0),
      *         ISNULL(LTCFILE.LTC_SENT_FLAG_1,''),
      *         ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_1
      *                                  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(LTCFILE.LTC_DESC_2,''),
      *         ISNULL(LTCFILE.LTC_MAKE_2,''),
      *         ISNULL(LTCFILE.LTC_MODEL_2,''),
      *         ISNULL(LTCFILE.LTC_YEAR_2,''),
      *         ISNULL(LTCFILE.LTC_STYLE_2,''),
      *         ISNULL(LTCFILE.LTC_CLASS_2,''),
      *         ISNULL(LTCFILE.LTC_VIN_2,''),
      *         ISNULL(LTCFILE.LTC_TITLE_NO_2,''),
      *         ISNULL(LTCFILE.LTC_MILEAGE_2, 0),
      *         ISNULL(LTCFILE.LTC_VALUE_2, 0),
      *         ISNULL(LTCFILE.LTC_SELLPR_2, 0),
      *         ISNULL(LTCFILE.LTC_MISC_AMT_2, 0),
      *         ISNULL(LTCFILE.LTC_SENT_FLAG_2,''),
      *         ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_2
      *                                  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(LTCFILE.LTC_DESC_3,''),
      *         ISNULL(LTCFILE.LTC_MAKE_3,''),
      *         ISNULL(LTCFILE.LTC_MODEL_3,''),
      *         ISNULL(LTCFILE.LTC_YEAR_3,''),
      *         ISNULL(LTCFILE.LTC_STYLE_3,''),
      *         ISNULL(LTCFILE.LTC_CLASS_3,''),
      *         ISNULL(LTCFILE.LTC_VIN_3,''),
      *         ISNULL(LTCFILE.LTC_TITLE_NO_3,''),
      *         ISNULL(LTCFILE.LTC_MILEAGE_3, 0),
      *         ISNULL(LTCFILE.LTC_VALUE_3, 0),
      *         ISNULL(LTCFILE.LTC_SELLPR_3, 0),
      *         ISNULL(LTCFILE.LTC_MISC_AMT_3, 0),
      *         ISNULL(LTCFILE.LTC_SENT_FLAG_3,''),
      *         ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_3
      *                                  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(LTCFILE.LTC_HHGS_1,''),
      *         ISNULL(LTCFILE.LTC_HHGS_2,''),
      *         ISNULL(LTCFILE.LTC_HHGS_3,''),
      *         ISNULL(LTDFILE.LTD_BRNO, 0),
      *         ISNULL(LTDFILE.LTD_ACCTNO, 0),
      *         ISNULL(LTDFILE.LTD_SEQNO, 0),
      *         ISNULL(LTDFILE.LTD_DEBIT_CARD_NUMBER, 0),
      *         ISNULL(LTDFILE.LTD_DEBIT_CARD_AMT, 0),
      *         ISNULL(CAST(LTDFILE.LTD_LOANDATE     
      *                                  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(CAST(LTDFILE.LTD_VOID_DATE    
      *                                  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(CAST(LTDFILE.LTD_CLEARED_DATE 
      *                                  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(LTDFILE.LTD_CLEARED_AMT, 0) 
      *
      *    FROM
      *
      * (SELECT LNFILE.LN_OWNBR,
      *         LNFILE.LN_ACCTNO,
      *         LNFILE.LN_CLASS,
      *         LNFILE.LN_PLDATE,
      *         LNFILE.LN_PLCD,
      *         LNFILE.LN_PLTYPE,
      *         LNFILE.LN_DATE_PAID_LAST,
      *         LNFILE.LN_CURBAL,
      *         LNFILE.LN_INTBAL,  
      *         LNFILE.LN_LCBAL,  
      *         LNFILE.LN_OTHBAL,
      *         LNFILE.LN_POFFDATE,
      *         LNFILE.LN_PAY_SCHLD_FG,
      *         LNFILE.LN_ORGST,   
      *         LNFILE.LN_SPRCLASS,
      *         LNFILE.LN_SUBCLASS,
      *         LNFILE.LN_LAWCODE, 
      *         LNFILE.LN_MAKERCD1, 
      *         CASE WHEN @QWS_PL           = 'O'
      *               AND LNFILE.LN_PLDATE  = '1900-01-01'
      *              THEN 'N'
      *     		   WHEN @QWS_PL           = 'S'
      *               AND LNFILE.LN_PLDATE != '1900-01-01'
      *              THEN 'N' 
      *              WHEN LNFILE.LN_PLDATE != '1900-01-01'
      *               AND @QWS_PLACTIVE     = 'A'
      *               AND LNFILE.LN_PLCD   != 'P'
      *              THEN 'N' 
      *              WHEN LNFILE.LN_PLDATE != '1900-01-01'
      *               AND @QWS_PLACTIVE     = 'I'
      *               AND LNFILE.LN_PLCD   != 'I'
      *               AND LNFILE.LN_PLCD   != 'W'
      *              THEN 'N'    
      *              WHEN LNFILE.LN_PLDATE != '1900-01-01'
      *               AND @QWS_PLTYPE      != 'A'
      *               AND LNFILE.LN_PLTYPE != @QWS_PLTYPE
      *              THEN 'N'
      *              WHEN LNFILE.LN_PLDATE != '1900-01-01'
      *               AND LNFILE.LN_PLDATE  < @QWS_PL_FROM_DATE
      *               AND LNFILE.LN_DATE_PAID_LAST 
      *                                     = '1900-01-01'
      *              THEN 'N'
      *              WHEN LNFILE.LN_PLDATE != '1900-01-01'
      *               AND LNFILE.LN_PLDATE  < @QWS_PL_FROM_DATE
      *               AND LNFILE.LN_DATE_PAID_LAST 
      *                                     < @QWS_TEST_DATE_PAID_LAST
      *              THEN 'N'
      *              WHEN @QWS_ZBAL         = 'O'
      *              AND (LNFILE.LN_CURBAL != 0 
      *                OR LNFILE.LN_INTBAL != 0 
      *                OR LNFILE.LN_LCBAL  != 0 
      *                OR LNFILE.LN_OTHBAL != 0 )
      *              THEN 'N'
      *              WHEN @QWS_ZBAL         = 'S'
      *              AND (LNFILE.LN_CURBAL  = 0 
      *              AND  LNFILE.LN_INTBAL  = 0 
      *              AND  LNFILE.LN_LCBAL   = 0 
      *              AND  LNFILE.LN_OTHBAL  = 0 )
      *              THEN 'N'
      *              WHEN LNFILE.LN_POFFDATE != '1900-01-01'
      *               AND @QWS_ZBAL_DATE     != @QWS_JULIAN_BEG
      *               AND LNFILE.LN_POFFDATE < @QWS_ZBAL_DATE
      *              THEN 'N'
      *              ELSE 'Y'
      *          END  AS USE_SW     
      *
      *    FROM DBO.LNFILE  LNFILE
      *
      *   WHERE    LNFILE.LN_OWNBR            = @QLN3_WBEG_OWNBR
      *     AND    LNFILE.LN_CLASS      BETWEEN @QLN3_WBEG_CLASS
      *                                     AND @QLN3_WEND_CLASS) TEMP 
      *
      *    LEFT OUTER JOIN
      *           DBO.LSMFILE  LSMFILE
      *      ON (  TEMP.LN_OWNBR            = LSMFILE.LSM_BRNO   
      *     AND    TEMP.LN_ACCTNO           = LSMFILE.LSM_ACCTNO
      *     AND    LSMFILE.LSM_SEQNO          = 1 )
      *
      *    LEFT OUTER JOIN
      *           DBO.LTBFILE  LTBFILE
      *      ON (  TEMP.LN_OWNBR            = LTBFILE.LTB_BRNO   
      *     AND    TEMP.LN_ACCTNO           = LTBFILE.LTB_ACCTNO
      *     AND    LTBFILE.LTB_SEQNO        = 1 ) 
      *
      *    LEFT OUTER JOIN
      *           DBO.LTCFILE  LTCFILE
      *      ON (  TEMP.LN_OWNBR            = LTCFILE.LTC_BRNO   
      *     AND    TEMP.LN_ACCTNO           = LTCFILE.LTC_ACCTNO
      *     AND    LTCFILE.LTC_SEQNO        = 1 ) 
      *
      *    LEFT OUTER JOIN
      *           DBO.LTDFILE  LTDFILE
      *      ON (  TEMP.LN_OWNBR            = LTDFILE.LTD_BRNO   
      *     AND    TEMP.LN_ACCTNO           = LTDFILE.LTD_ACCTNO
      *     AND    LTDFILE.LTD_SEQNO        = 1 ) 
      *
      *   WHERE (( TEMP.USE_SW = 'Y' )
      *     AND  ( LSM_ACCTNO != 0
      *      OR    LTB_ACCTNO != 0 
      *      OR    LTC_ACCTNO != 0 
      *      OR    LTD_ACCTNO != 0 ))
      *
      *   ORDER BY TEMP.LN_OWNBR,
      *            TEMP.LN_CLASS,
      *            TEMP.LN_ACCTNO;

           MOVE BR-NO                   TO QLN3-WBEG-OWNBR.
           MOVE BEG-CLASS               TO QLN3-WBEG-CLASS.
           MOVE END-CLASS               TO QLN3-WEND-CLASS.
           MOVE ZBAL                    TO QWS-ZBAL.
           MOVE PL                      TO QWS-PL.
           MOVE PLACTIVE                TO QWS-PLACTIVE.
           MOVE PLTYPE                  TO QWS-PLTYPE.
           MOVE PL-FROM-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE. 
           MOVE SQL-DATE-YYYY-MM-DD     TO QWS-PL-FROM-DATE.
           MOVE ZBAL-DATE               TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE. 
           MOVE SQL-DATE-YYYY-MM-DD     TO QWS-ZBAL-DATE.
           MOVE EXT-JULIAN-BEG          TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE. 
           MOVE SQL-DATE-YYYY-MM-DD     TO QWS-JULIAN-BEG.
           MOVE TEST-DATE-PAID-LAST     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE. 
           MOVE SQL-DATE-YYYY-MM-DD     TO QWS-TEST-DATE-PAID-LAST.

           EXEC SQL SET CONNECTION LN3_LSLT_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_LSLT_CURSOR CURSOR FOR
               :LN3-RETURN-CODE = EXEC SSP_LTEXTR_LNFILE_LSLTFILE_SELECT 
             ( :QLN3-WBEG-OWNBR,
               :QLN3-WBEG-CLASS, 
               :QLN3-WEND-CLASS, 
               :QWS-ZBAL,               
               :QWS-PL,                 
               :QWS-PLACTIVE,           
               :QWS-PLTYPE,             
               :QWS-PL-FROM-DATE,       
               :QWS-ZBAL-DATE,          
               :QWS-JULIAN-BEG,         
               :QWS-TEST-DATE-PAID-LAST )
           END-EXEC.

           EXEC SQL
               OPEN LN3_LSLT_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

      *    MOVE ZEROES TO QLN3-WBEG-OWNBR
      *                   QLN3-WEND-OWNBR
      *                   QLN3-WBEG-CLASS
      *                   QLN3-WEND-CLASS
      *                   QLN3-WBEG-ACCTNO
      *                   QLN3-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-LN3-LSLT-FILE-NEXT. *> EMBEDDED - LN, LSM, LTB, LTC, LTD
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.
           INITIALIZE QLSM-REC.
           INITIALIZE QLTB-REC.
           INITIALIZE QLTC-REC.
           INITIALIZE QLTD-REC.

           EXEC SQL SET CONNECTION LN3_LSLT_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH LN3_LSLT_CURSOR 
                  INTO 
                    :QLN-OWNBR,
                    :QLN-ACCTNO,
                    :QLN-CLASS,
                    :QLN-PLDATE,
                    :QLN-PLCD,
                    :QLN-PLTYPE,
                    :QLN-DATE-PAID-LAST,
                    :QLN-CURBAL,
                    :QLN-INTBAL,  
                    :QLN-LCBAL,  
                    :QLN-OTHBAL,
                    :QLN-POFFDATE,
                    :QLN-PAY-SCHLD-FG,
                    :QLN-ORGST,   
                    :QLN-SPRCLASS,
                    :QLN-SUBCLASS,
                    :QLN-LAWCODE, 
                    :QLN-MAKERCD1,
                    :QLSM-BRNO,
                    :QLSM-ACCTNO,
                    :QLSM-SEQNO,
                    :QLSM-MISC-FIELD-1, 
                    :QLSM-MISC-FIELD-2, 
                    :QLSM-MISC-FIELD-3, 
                    :QLSM-MISC-FIELD-4, 
                    :QLSM-MISC-FIELD-5, 
                    :QLSM-MISC-FIELD-6, 
                    :QLSM-MISC-FIELD-7, 
                    :QLSM-MISC-FIELD-8, 
                    :QLSM-MISC-FIELD-9, 
                    :QLSM-MISC-FIELD-10, 
                    :QLSM-MISC-FIELD-11, 
                    :QLSM-MISC-FIELD-12, 
                    :QLSM-MISC-FIELD-13, 
                    :QLSM-MISC-FIELD-14, 
                    :QLSM-MISC-FIELD-15, 
                    :QLSM-MISC-FIELD-16, 
                    :QLSM-MISC-FIELD-17, 
                    :QLSM-MISC-FIELD-18, 
                    :QLSM-MISC-FIELD-19, 
                    :QLSM-MISC-FIELD-20, 
                    :QLSM-MISC-FIELD-21, 
                    :QLSM-MISC-FIELD-22, 
                    :QLSM-MISC-FIELD-23, 
                    :QLSM-MISC-FIELD-24, 
                    :QLSM-MISC-FIELD-25, 
                    :QLSM-MISC-FIELD-26, 
                    :QLSM-MISC-FIELD-27, 
                    :QLSM-MISC-FIELD-28, 
                    :QLSM-MISC-FIELD-29, 
                    :QLSM-MISC-FIELD-30, 
                    :QLSM-MISC-FIELD-31, 
                    :QLSM-MISC-FIELD-32, 
                    :QLSM-TRANS-FROM-BRNO,  
                    :QLSM-TRANS-FROM-ACCTNO, 
                    :QLSM-TRANS-FROM-DATE,
                    :QLSM-MISPAY-90-DATE,
                    :QLSM-SECOND-BEN-NAME,
                    :QLSM-SECOND-BEN-ADR,
                    :QLSM-SECOND-BEN-CITY,
                    :QLSM-SECOND-BEN-STATE,
                    :QLSM-SECOND-BEN-ZIP,
                    :QLSM-SECOND-BEN-PHONE,
                    :QLSM-SECOND-BEN-RELATION,
                    :QLTB-BRNO, 
                    :QLTB-ACCTNO,  
                    :QLTB-SEQNO,  
                    :QLTB-FILEDATE,
                    :QLTB-CASE-NO, 
                    :QLTB-CHAPTER, 
                    :QLTB-MISC1, 
                    :QLTB-MISC2, 
                    :QLTB-MISC3, 
                    :QLTB-BDDATE,
                    :QLTB-WHO-FILED, 
                    :QLTB-ENDORSER-FILED, 
                    :QLTB-CLAIM-AMT,  
                    :QLTB-ACTIVE-INACTIVE, 
                    :QLTB-INT-LC-DUE,  
                    :QLTB-OTH-DUE,  
                    :QLTB-TDUEAMT,  
                    :QLTB-NAME-1, 
                    :QLTB-NAME-2, 
                    :QLTB-NAME-3, 
                    :QLTB-ADR1-1, 
                    :QLTB-ADR1-2, 
                    :QLTB-ADR1-3, 
                    :QLTB-ADR2-1, 
                    :QLTB-ADR2-2, 
                    :QLTB-ADR2-3, 
                    :QLTB-CITY-1, 
                    :QLTB-CITY-2, 
                    :QLTB-CITY-3, 
                    :QLTB-STATE-1, 
                    :QLTB-STATE-2, 
                    :QLTB-STATE-3, 
                    :QLTB-ZIP-1, 
                    :QLTB-ZIP-2, 
                    :QLTB-ZIP-3, 
                    :QLTB-PHONE-1,  
                    :QLTB-PHONE-2,  
                    :QLTB-PHONE-3,  
                    :QLTB-REAFF-DATE,
                    :QLTB-REAFF-SYSDATE,
                    :QLTB-REAFF-BALANCE,   
                    :QLTB-REAFF-PAYAMT,  
                    :QLTB-REAFF-NO-PAYS,  
                    :QLTB-INTRATE, 
                    :QLTB-RESCIND-DATE,
                    :QLTB-PARTIAL-FULL,
                    :QLTB-DISCHARGE-DATE,
                    :QLTB-DISCHARGE-SYSDATE,
                    :QLTC-BRNO,  
                    :QLTC-ACCTNO,  
                    :QLTC-SEQNO,  
                    :QLTC-DESC-1, 
                    :QLTC-MAKE-1, 
                    :QLTC-MODEL-1, 
                    :QLTC-YEAR-1, 
                    :QLTC-STYLE-1, 
                    :QLTC-CLASS-1, 
                    :QLTC-VIN-1, 
                    :QLTC-TITLE-NO-1, 
                    :QLTC-MILEAGE-1,  
                    :QLTC-VALUE-1,  
                    :QLTC-SELLPR-1,  
                    :QLTC-MISC-AMT-1,  
                    :QLTC-SENT-FLAG-1, 
                    :QLTC-VIN-CHG-DATE-1,
                    :QLTC-DESC-2, 
                    :QLTC-MAKE-2, 
                    :QLTC-MODEL-2, 
                    :QLTC-YEAR-2, 
                    :QLTC-STYLE-2, 
                    :QLTC-CLASS-2, 
                    :QLTC-VIN-2, 
                    :QLTC-TITLE-NO-2, 
                    :QLTC-MILEAGE-2,  
                    :QLTC-VALUE-2, 
                    :QLTC-SELLPR-2, 
                    :QLTC-MISC-AMT-2, 
                    :QLTC-SENT-FLAG-2, 
                    :QLTC-VIN-CHG-DATE-2,
                    :QLTC-DESC-3, 
                    :QLTC-MAKE-3, 
                    :QLTC-MODEL-3, 
                    :QLTC-YEAR-3, 
                    :QLTC-STYLE-3, 
                    :QLTC-CLASS-3, 
                    :QLTC-VIN-3, 
                    :QLTC-TITLE-NO-3, 
                    :QLTC-MILEAGE-3, 
                    :QLTC-VALUE-3,  
                    :QLTC-SELLPR-3,  
                    :QLTC-MISC-AMT-3,  
                    :QLTC-SENT-FLAG-3, 
                    :QLTC-VIN-CHG-DATE-3,
                    :QLTC-HHGS-1, 
                    :QLTC-HHGS-2, 
                    :QLTC-HHGS-3, 
                    :QLTD-BRNO,   
                    :QLTD-ACCTNO, 
                    :QLTD-SEQNO,  
                    :QLTD-DEBIT-CARD-NUMBER, 
                    :QLTD-DEBIT-CARD-AMT,  
                    :QLTD-LOANDATE,
                    :QLTD-VOID-DATE,
                    :QLTD-CLEARED-DATE,
                    :QLTD-CLEARED-AMT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-LSLT-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS 
              PERFORM GET-LSM-FIELDS 
              PERFORM GET-LTB-FIELDS 
              PERFORM GET-LTC-FIELDS
              PERFORM GET-LTD-FIELDS. 

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.
           PERFORM CLOSE-IT.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_LSLT_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_LSLT_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-LSLT-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LN3_LSLT_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LN3_LSLT_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LN3-LSLT-CONN-OPEN-SW.

      *-----------------------------------------------------------------

       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPNO1RN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
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
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME
           ELSE
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE SPACES TO MESS
                 PERFORM SEND-LEGEND.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LTEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
