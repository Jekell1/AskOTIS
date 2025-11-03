      * COPYMEMBER: LIBLP/LP01LT
      ******************************************************************
      *    LOAN TRAILER FILE : (144) BYTES
      *
      * COLLATERAL INFO:
      *    LT-SEQNO - 1 THRU 4 RESERVED FOR COLLATERAL IF LT-CODE = "C"
      *    LT-TYPE  - 1 DESIGNATES USING LT-COLLATERAL-INFO REDEFINITION
      *               2 DESIGNATES USING LT-COLLATERAL-INFO.
      *
      *  CODE   SEQNO   TYPE    DESCRIPTION
      *    C      1       1       AUTO 1
      *           2       1       AUTO 2
      *           3       2       GOODS AND APPL 1
      *           4       2       GOODS AND APPL 2
      *           5       2       GOODS AND APPL 3
      *---------------------------------------------------------------
      *  CODE   SEQNO   TYPE    DESCRIPTION
      *    D     1       N/A       DEBIT CARD PROCEEDS FILE
      *---------------------------------------------------------------
      * LIENHOLDER INFO:
      *  CODE   SEQNO   TYPE    DESCRIPTION
      *    L      1       0       LIENHOLDER 1
      *           2       0       LIENHOLDER 2
      *           3       0       LIENHOLDER 3
      *           4       0       LIENHOLDER 4 (COMMENTS)
      *           5       0       PAYOFF TITLE LIENHOLDER
      *           6       0       PAYOFF TITLE LIENHOLDER (COMMENTS)
      *---------------------------------------------------------------
      * INSURANCE INFO:
      *  CODE   SEQNO   TYPE    DESCRIPTION
      *    I             1-5     INSURANCE LEVEL OF LOAN RECORD
      *
      *---------------------------------------------------------------
      * PAYMENT SCHEDULE INFO:
      *  CODE   SEQNO   TYPE   
      *    P      1     N/A     ONE RECORD PER LOAN
      *
      *---------------------------------------------------------------
      * BANKRUPT INFO:
      *  CODE   SEQNO   TYPE
      *    B      1     N/A     GENERAL INFO
      *           2     N/A     DEBTOR'S ATTORNEY
      *           3     N/A     CLERK OF BANKRUPTCY COURT
      *           4     N/A     TRUSTEE
      *           5     N/A     REAFFIRMATION AND DISCHARGE
      *
      *---------------------------------------------------------------
      * AUTO INSURANCE INFO:
      *    LT-SEQNO - TO KEEP AUTO INS RECS UNIQUE IF MORE THAN 1
      *    LT-TYPE  - DESIGNATES DIFFERENT RECORD INFO
      *
      *  CODE   SEQNO   TYPE    DESCRIPTION
      *    A      1       0       AGENCY INFO
      *           2       0       COVERAGE INFO
      *           3       0       MISC.
      *
      *---------------------------------------------------------------
      * FEE CODE PAYABLE (COMMISSION)
      *  CODE   SEQNO   TYPE    DESCRIPTION
      *    F      1     N/A     FEE CODE PAYABLE - SUCH AS NON-FILE
      *
      *---------------------------------------------------------------
      * ADDITIONAL LOAN INFO:
      *  CODE   SEQNO   TYPE    DESCRIPTION
      *    G      1     N/A     GENERAL ADDITIONS
      *                         - DEALER RESERVE BALANCES
      *                         - INSURANCE REFUNDS
      *
      *---------------------------------------------------------------
      * REV:
      *  JTG 081590 SIGNED INS INFO IN "I" RECORD
      *                           + ADDED LT-INT-LAST-EARNED
      *                           + ADDED LT-INS-REBATE
      *  JTG 032293 CHANGED LT-PAY-SCHLD-TBL FROM 20 TO OCCURS 17 TIMES.
      *                 AND LT-SCHLD-TERM FROM PIC 99 TO  PIC 999  COMP.
      *  BAH 060195 CHANGED LT-COLL-AMT1 TO LT-MILEAGE
      *                     LT-COLL-AMT2 TO LT-VALUE
      *                     LT-COLL-AMT3 TO LT-SELLPR
      *                     LT-COLL-AMT4 TO LT-MISC-AMT
      *  JTG 070296 ADDED LT-B-CHAPTER-X    REDEFINES LT-B-CHAPTER PIC XX
      *  BAH 071196 ADDED AUTO INS RECORD TYPE 'A' FROM REL A50, INSTANT
      *  BAH 071196 ADDED ADDTL G/L RECORD TYPE 'G' FROM REL A50
      *  JTG 052797 CREATED LT-ALPHA FOR COLLATERAL ALPHA AREA RE: WI/MAKERS.C
      *  JTG 112097 EXPANDED RECORD BY 30 BYTES
      *  MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  KEC 990510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 990811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  BAH 020923 ADDED LT-B-BDDATE, BANKRUPTCY DISMISSAL DATE,
      *             REGACC PR#1901
      *  JTG 030212 ADDONS CANCELLED VIA ADDON MODULE DO INCORRECT ADDITIONAL
      *             REFUND ON THE 'LT' INTEREST                    
      *
      *             CHANGED VALUES OF LT-INS-REBATE FLAG SUCH THAT WHEN:
      *               LT-INS-REBATE = " "  NO REFUND OR CANCEL OCCURED
      *                             = "Y"  INS. WAS CANCELLED VIA ADDON POSTING
      *                             = "R"  INS. WAS REBATED   VIA TRANS POSTING
      *             ADDED LT-INS-REBATE-VALID ' ' 'Y' 'R'             MULL #382
      *  BAH 030407 ADDED LT-CODE "L" SEQ 1-3 FOR LIENHOLDER INFO REGACC PR# 20
      *  BAH 030625 ADDED LT-SEQ 4 FOR LIENHOLDER COMMENTS REGACC PR# 32
      *  BAH 040107 ADDED LT-CODE "F" FOR FEECODE PAYABLE PERCENT (COMMISSION)
      *             FOR NON-FILE WORLD PR# 331
      *  BAH 040305 ADDED LT-B-WHO-FILED, WHO FILED BANKRUPTCY  REGACC PR# 51
      *  BAH 040412 ADDED LT-SEQ 5 & 6 TO LIENHOLDER FOR PAYOFF TITLE PROCESSING
      *              REGACC PR# 53
      *  BAH 040520 ADDED ADDITIONAL FIELDS FOR TITLE PROCESSING, REGACC PR#60
      *             LT-L-TITLE-RCV-DATE,LT-L-TITLE-LIEN-DATE,LT-L-TITLE-NO,
      *             LT-L-LIEN-LETTER-SENT
      *  BAH 040803 ADDED LT-L-SEND-ASAP TO TITLE PROCESSING FOR REGACC PR#58
      *  BAH 051104 ADDED LT-B-ENDORSER-FILED FOR METRO2, MULLEN PR# 2645
      *  BAH 070501 ADDED LT-A-TRACK-ONLY, LT-A-INSURANCE, LT-A-TRACK-MONTHS,
      *             LT-A-INS-MONTHS AND LT-A-CPP-MONTHS FOR INSURANCE
      *             TRACKING, REGACC PR# 173
      *  BAH 080221 ADDED LT-B-CLAIM-AMT FOR PAYOFF AMT + ALL INS REFUNDS
      *             AUTOMATICALLY FILLED IN LONPF2 "BK", WORLD PR# 570
      *  BAH 080415 ADDED LT-SENT-FLAG FOR A VIN/LIEN EXTRACTION TO
      *             SKIP AND INCLUDE ALREADY SENT REGENCY REGWAN PR# 3179
      *   CS 080707 ADD LT-L-TITLE-SEND-FMNO REGACC PR#187
      *  BAH 081013 ADDED LT-B-ACTIVE-INACTIVE FLAG FOR REGACC PR# 223
      *  BAH 100819 ADDED LT-L-TITLE-ADR2 FOR REGACC PR# 270
      *  BAH 101013 ADDED LT-B-INT-LC-DUE, LT-B-OTH-DUE AND LT-B-TDUEAMT
      *             FOR PROOF OF CLAIM FORMS REQUIRED, WORLD PR# 774
      *  BAH 120904 ADDED LT-VIN-CHG-DATE TO TRACK VIN CHANGES, REGACC PR# 319
      *  BAH 120905 ADDED LT-L-LIEN-TITLE-STATE AND LT-L-RELEASE-TYPE
      *                                                         REGACC PR# 319
      *  BAH 120924 ADDED LT-L-TITLE-STATUS-CODE REGACC PR# 321
      *  BAH 120928 ADDED LT-L-CARRIER-TYPE,LT-L-CARRIER-ACCTNO REGACC PR# 322
      *   CS 150721 ADDED LT-CANCEL-REASON-CD   KSTREET PR#4770
      *  BAH 160427 ADDED LT SEQUENCE 5 FOR PAGE 2 BANKRUPTCY
      *             REAFFIRMATION AND DISCHARGE INFO           WORLD PR#1150
      *  BAH 180220 ADDED "D" DEBIT CARD FILE FOR NETSPEND, WORLD #1306
      *  BAH 200925 ADDED LT-B-GROSS-BAL, TO HOLD BK CURBAL  #1461
      **************************************************************************
       78  LT-SIZE                       VALUE 144.
       01  LT-REC.
           03  LT1-KEY.
               05  LT-ACCTNO             PIC 9(8) COMP-3.
               05  LT-CODE               PIC X.
               05  LT-SEQNO              PIC 999  COMP-3.

           03  LT-TYPE                   PIC 99   COMP.
           03  LT-REDEFINE-AREA          PIC X(135).
      *---------------------------------------------------------------
      *    COLLATERAL RECORD "C":
      *---------------------------------------------------------------
           03  LT-COLLATERAL-AREA        REDEFINES LT-REDEFINE-AREA.
               05  LT-COLLATERAL-INFO    PIC X(135).
               05  FILLER                REDEFINES LT-COLLATERAL-INFO.
                   07  LT-COLL-INFO1     PIC X(20).
                   07  LT-COLL-INFO2     PIC X(20).
                   07  LT-COLL-INFO3     PIC X(20).
                   07  LT-COLL-INFO4     PIC X(20).
                   07  FILLER            PIC X(55).
               05  FILLERX               REDEFINES LT-COLLATERAL-INFO.
                   07  LT-COLL-BUF       PIC X OCCURS 80.
                   07  FILLER            PIC X(55).
               05  FILLER                REDEFINES LT-COLLATERAL-INFO.
                   07  LT-ALPHA.
                       09  LT-DESCRIPTION    PIC X(10).
                       09  LT-MAKE           PIC X(15).
                       09  LT-MODEL          PIC X(15).
                       09  LT-YEAR           PIC X(4).
                       09  LT-STYLE          PIC X(8).
                       09  LT-CLASS          PIC X(8).
                       09  LT-ID             PIC X(20).
                       09  LT-VIN REDEFINES LT-ID PIC X(20).
                       09  LT-TITLE-NO       PIC X(8).
                   07  LT-MILEAGE            PIC S9(7)    COMP-3.
                   07  LT-VALUE              PIC S9(5)V99 COMP-3.
                   07  LT-SELLPR             PIC S9(5)V99 COMP-3.
                   07  LT-MISC-AMT           PIC S9(5)V99 COMP-3.
                   07  LT-SENT-FLAG          PIC X.
                   07  LT-VIN-CHG-DATE       PIC 9(6)     COMP-6.
                   07  FILLER                PIC X(27).
      *---------------------------------------------------------------
      *    INSURANCE RECORD "I":
      *---------------------------------------------------------------
           03  LT-INSURANCE-AREA         REDEFINES LT-REDEFINE-AREA.
               05  LT-ORIG-1STPAY        PIC 9(6)      COMP.
               05  LT-PRIOR-NODEF        PIC S99       COMP.
               05  LT-FEES               OCCURS 2 TIMES.
                   07  LT-INSFEECD       PIC XX.
                   07  LT-INSFEE         PIC 9(4)V99   COMP.
               05  LT-TRCD               PIC X(2).
               05  LT-INSTBL.
                   07  LT-INSCOMP        PIC 99        COMP.
                   07  LT-INSOURS        PIC X.
                   07  LT-INSEFF         PIC 9(6)      COMP.   
                   07  LT-INSTRM         REDEFINES LT-INSEFF
                                         PIC 9(6)      COMP.
                   07  LT-INSEXP         PIC 9(6)      COMP.   
                   07  LT-INSPREM        PIC S9(4)V99  COMP.
                   07  LT-INSCOVR        PIC S9(6)V99  COMP.
                   07  LT-INSCOMM        PIC S9(4)V99  COMP.
               05  LT-ANTICTERM          PIC 9(3)      COMP.
      * 1 INSURANCE INTEREST (USED IN ACR PROGRAMS)
      * 2 NOT USED
      * 3 LT-ANTICERN(3) HOLDS ORIGINAL ADDON FINCHG 
               05  LT-ANTICTBL           OCCURS 3 TIMES.
                   07  LT-ANTICERN       PIC S9(6)V99  COMP.   
                   07  LT-ANTICADJ       PIC S9(6)V99  COMP.   
                   07  LT-UFERN          PIC S9(6)V99  COMP.   
                   07  LT-ACCERN         PIC S9(6)V99  COMP.   
               05  LT-INT-REBATE         PIC X.
               05  LT-INS-PAYMNT-INCR    PIC S9(5)V99  COMP.
               05  LT-INS-TOT-ADDON      PIC S9(5)V99  COMP.
               05  LT-INT-LAST-EARNED    PIC S9(5)V99  COMP.
               05  LT-INS-REBATE         PIC X.
382            88  LT-INS-REBATE-VALID   VALUE " " "Y" "R".
               05  LT-RB-WORKER          PIC S9(5)V99  COMP.
               05  LT-ANTIC-START-DATE   PIC 9(6)      COMP.
               05  LT-CANCEL-REASON-CD   PIC X(3).
               05  FILLER                PIC X(27).
      *---------------------------------------------------------------
      * PAYMENT SCHEDULE RECORD "P":
      *---------------------------------------------------------------
           03  LT-PAY-SCHLD-AREA         REDEFINES LT-REDEFINE-AREA.
               05  LT-PAY-SCHLD-TBL      OCCURS 17 TIMES.
                   07  LT-SCHLD-TERM     PIC 999     COMP.
                   07  LT-SCHLD-PAYMNT   PIC 9(6)V99 COMP.
               05  FILLER                PIC X(33).
      *---------------------------------------------------------------
      * FEECODE PAYABLE RECORD "F":
      *---------------------------------------------------------------
           03  LT-FEECODE-PAYABLE-AREA   REDEFINES LT-REDEFINE-AREA.
               05  LT-FEECODE-TBL        OCCURS 10 TIMES.
                   07  LT-FEECODE         PIC XX.
                   07  LT-FEECODE-PAYABLE PIC S9(5)V99 COMP-3.

               05  FILLER                PIC X(75).
      *---------------------------------------------------------------
      *    ADDITIONAL GENERAL LOAN INFO RECORD "G":
      *        NOTE:
      *             REMEMBER TO CHANGE LIBCL/UPDLTG
      *             IF LT-G-RECORD1 IS CHANGED!!!!!!
      *---------------------------------------------------------------
           03  LT-G-RECORD1              REDEFINES LT-REDEFINE-AREA.
      * LT-G-DL..RV-BAL = CURRENT BALANCE ON RESERVE:
               05  LT-G-DLRV-BAL         PIC S9(6)V99   COMP.
               05  LT-G-DLSPRV-BAL       PIC S9(6)V99   COMP.
               05  LT-G-DLPARV-BAL       PIC S9(6)V99   COMP.
      * LT-G-INSADG = REAL INS. REFUNDS:
               05  LT-G-INSADJ           OCCURS 5
                                         PIC S9(6)V99   COMP.
      * LT-G-EARNED = EARNINGS TODATE:
               05  LT-G-EARNED           OCCURS 10
                                         PIC S9(6)V99   COMP.
      * LT-G-EARNED-FEES = EARNINGS TODATE ON ALL LOAN FEES:
               05  LT-G-EARNED-FEES      PIC S9(6)V99   COMP.
      * LT-G-RESERVE-PAYMENTS = PAYMENTS MADE TO RESERVE
               05  LT-G-RESERVE-PAYMENTS PIC S9(6)V99   COMP.
               05  LT-G-FILLER           PIC X(55).
      *---------------------------------------------------------------
      *    BANKRUPT RECORD "B":
      *---------------------------------------------------------------
           03  LT-BANKRUPT-AREA          REDEFINES LT-REDEFINE-AREA.
               05  LT-B-FILEDATE         PIC 9(6)  COMP-3.
               05  LT-B-CASE-NO          PIC X(9).
               05  LT-B-CHAPTER          PIC 99.
               05  LT-B-CHAPTER-X       REDEFINES LT-B-CHAPTER
                                         PIC XX.
               05  LT-B-MISC1            PIC X(30).
               05  LT-B-MISC2            PIC X(30).
               05  LT-B-MISC3            PIC X(30).
               05  LT-B-BDDATE           PIC 9(6)  COMP-3.
               05  LT-B-WHO-FILED        PIC X.
               05  LT-B-ENDORSER-FILED   PIC X.
               05  LT-B-CLAIM-AMT        PIC S9(7)V99   COMP-3.
               05  LT-B-ACTIVE-INACTIVE  PIC X.
               05  LT-B-INT-LC-DUE       PIC S9(7)V99   COMP-3.
               05  LT-B-OTH-DUE          PIC S9(7)V99   COMP-3.
               05  LT-B-TDUEAMT          PIC S9(7)V99   COMP-3.
               05  FILLER                PIC X(3).
           03  LT-B-ADDR-AREA            REDEFINES LT-REDEFINE-AREA.
               05  LT-B-NAME             PIC X(24).
               05  LT-B-ADR1             PIC X(24).
               05  LT-B-ADR2             PIC X(24).
               05  LT-B-CITY             PIC X(15).
               05  LT-B-STATE            PIC XX.
               05  LT-B-ZIP              PIC X(10).
               05  LT-B-PHONE            PIC 9(10)    COMP-3.
               05  FILLER                PIC X(30).
           03  LT-B-REAFF-DISCHARGE-AREA REDEFINES LT-REDEFINE-AREA.
               05  LT-B-REAFF-DATE        PIC 9(6)     COMP-3.
               05  LT-B-REAFF-SYSDATE     PIC 9(6)     COMP-3.
               05  LT-B-REAFF-BALANCE     PIC 9(6)V99  COMP-3.
               05  LT-B-REAFF-PAYAMT      PIC 9(6)V99  COMP-3.       
               05  LT-B-REAFF-NO-PAYS     PIC 9(3).
               05  LT-B-INTRATE           PIC 9(3)v9(3).
               05  LT-B-RESCIND-DATE      PIC 9(6)     COMP-3.
               05  LT-B-PARTIAL-FULL      PIC X.
               05  LT-B-DISCHARGE-DATE    PIC 9(6)     COMP-3.
               05  LT-B-DISCHARGE-SYSDATE PIC 9(6)     COMP-3.
               05  LT-B-GROSS-BAL         PIC 9(6)V99  COMP-3.
               05  FILLER                 PIC X(90).
           
      *---------------------------------------------------------------
      *    LIENHOLDER INFO RECORD "L":
      *---------------------------------------------------------------
           03  LT-L-RECORD               REDEFINES LT-REDEFINE-AREA.
               05  LT-L-LIEN-TYPE        PIC 9.
               05  LT-L-LIEN-NAME        PIC X(30).
               05  LT-L-LIEN-ADR         PIC X(24).
               05  LT-L-LIEN-CITY        PIC X(15).
               05  LT-L-LIEN-STATE       PIC X(2).
               05  LT-L-LIEN-ZIP         PIC X(10).
               05  LT-L-LIEN-DATE        PIC 9(6).

               05  LT-L-TITLE-RCV-DATE   PIC 9(6) COMP-3.
               05  LT-L-TITLE-LIEN-DATE  PIC 9(6) COMP-3.
               05  LT-L-TITLE-NO         PIC X(20).
               05  LT-L-LIEN-LETTER-SENT PIC X.

               05  FILLER                PIC X(18).
      *---------------------------------------------------------------
           03  LT-L-RECORD-4             REDEFINES LT-REDEFINE-AREA.
               05  LT-L-LIEN-COMMENT1    PIC X(60).
               05  LT-L-LIEN-COMMENT2    PIC X(60).

               05  LT-L-LIEN-TITLE-STATE  PIC XX.
               05  LT-L-TITLE-STATUS-CODE PIC XX.
               05  FILLER                 PIC X(11).
      *---------------------------------------------------------------
           03  LT-L-RECORD-5             REDEFINES LT-REDEFINE-AREA.
               05  LT-L-TITLE-SEND-FLAG   PIC X.
               05  LT-L-TITLE-NAME        PIC X(25).
               05  LT-L-TITLE-ADR         PIC X(25).
               05  LT-L-TITLE-CITY        PIC X(15).
               05  LT-L-TITLE-STATE       PIC X(2).
               05  LT-L-TITLE-ZIP         PIC X(10).
               05  LT-L-TITLE-DATE        PIC 9(6).
               05  LT-L-TITLE-SENT-BY     PIC X(25).
               05  LT-L-SEND-ASAP         PIC X.
               05  LT-L-TITLE-SEND-FMNO   PIC 9(3).
               05  LT-L-TITLE-ADR2        PIC X(22).

      *---------------------------------------------------------------
           03  LT-L-RECORD-6             REDEFINES LT-REDEFINE-AREA.
               05  LT-L-TITLE-COMMENT1    PIC X(40).
               05  LT-L-TITLE-COMMENT2    PIC X(40).

               05  LT-L-RELEASE-TYPE      PIC XX.
               05  LT-L-CARRIER-TYPE      PIC X(4).
               05  LT-L-CARRIER-ACCTNO    PIC X(20).
               05  FILLER                 PIC X(29).
      *---------------------------------------------------------------
      *    AUTO INSURANCE RECORD "A":
      *---------------------------------------------------------------
           03  LT-A-RECORD1              REDEFINES LT-REDEFINE-AREA.
               05  LT-A-DLR-REQ          PIC X.
               05  LT-A-INS-TYPE         PIC X.
               05  LT-A-INS-CO           PIC X(25).
               05  LT-A-POLICY-NO        PIC X(25).
               05  LT-A-AGENCY           PIC X(25).
               05  LT-A-AGENT            PIC X(25).
               05  FILLER                PIC X(33).
           03  LT-A-RECORD2              REDEFINES LT-REDEFINE-AREA.
               05  LT-A-ADR1             PIC X(25).
               05  LT-A-ADR2             PIC X(25).
               05  LT-A-CITY             PIC X(15).
               05  LT-A-STATE            PIC XX.
               05  LT-A-ZIP              PIC X(10).
               05  LT-A-PHONE            PIC 9(10)    COMP-3.
               05  LT-A-EFF-DATE         PIC 9(6)     COMP-3.   
               05  LT-A-EXP-DATE         PIC 9(6)     COMP-3.   
               05  LT-A-CAN-DATE         PIC 9(6)     COMP-3.   
               05  LT-A-CONF-DATE        PIC 9(6)     COMP-3.   
               05  LT-A-LAST-FORM        PIC 9(3)     COMP-3.   
               05  FILLER                PIC X(34).
           03  LT-A-RECORD3              REDEFINES LT-REDEFINE-AREA.
               05  LT-A-COV-TYPE         PIC X.
               05  FILLER                PIC X(24).
               05  LT-A-BIL-COV          PIC S9(7)V99 COMP-3.   
               05  LT-A-PDL-COV          PIC S9(7)V99 COMP-3.   
               05  LT-A-PIP-COV          PIC S9(7)V99 COMP-3.   
               05  LT-A-UMB-COV          PIC S9(7)V99 COMP-3.   
               05  LT-A-COL-COV          PIC S9(7)V99 COMP-3.   
               05  LT-A-LOSS-PAYEE       PIC X(25).
               05  LT-A-COM-COV          PIC S9(7)V99 COMP-3.   
      * INSURANCE TRACKING FIELDS PR# 173
               05  LT-A-TRACK-ONLY       PIC X.
               05  LT-A-INSURANCE        PIC X.

               05  LT-A-TRACK-MONTHS     PIC X(12).
               05  LT-A-INS-MONTHS       PIC X(12).
               05  LT-A-CPP-MONTHS       PIC X(12).
               05  FILLER                PIC X(17).
               
      *---------------------------------------------------------------
      *    DEBIT CARD PROCEEDS RECORD "D":
      *---------------------------------------------------------------
           03  LT-DEBIT-CARD-AREA       REDEFINES LT-REDEFINE-AREA.
               05  LT-D-DEBIT-CARD-NUMBER PIC 9(10)    COMP-3.
               05  LT-D-DEBIT-CARD-AMT    PIC S9(7)V99 COMP-3.
               05  LT-D-LOANDATE          PIC 9(6)     COMP-3.
               05  LT-D-VOID-DATE         PIC 9(6)     COMP-3.
               05  LT-D-CLEARED-DATE      PIC 9(6)     COMP-3.
               05  LT-D-CLEARED-AMT       PIC S9(7)v99 COMP-3.
               
               05  FILLER                 PIC X(107).
              
