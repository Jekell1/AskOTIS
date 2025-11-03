      * COPYMEMBER: LIBLP/CHECKTRANSW
      *
      *  CS 120214 ADDED NEW PAYEE TYPE OF 'ID'[INTERIM DEPOSIT]. WORLD WANTED
      *            CASH DRAWER UPDATED WITHOUT FORCING THE USER TO DO A BK &
      *            THEN A CASH DISBURSEMENT. ADDED IF GP-I-AM-WORLD, FORCES 'BK'
      *            TO TRANS-TYPE & ALLOW AN INPUT OF 'BK' OR 'ID' TO PAYEE
      *            TYPE. IF 'BK' ACTS AS IT DOES TODAY & ALLOCATION WINDOW IS
      *            DISPLAYED. IF 'ID' PROMPTS FOR THE CASH DRAWER NUMBER TO
      *            BE UPDATED & FORCES THE BR-GLONHAND TO THE FIRST LEVEL OF
      *            GL ENTRY, NO ADDITIONAL ENTRIES ARE PERMITTED. IF YOU ARE
      *            NOT WORLD, PAYEE TYPE IS FORCED TO 'BK' AND NO ENTRY OF
      *            CASH DRAWER IS ALLOWED.  WORLD PR# 790
      *            ADDED NEW PAYEE TYPE OF 'WI'[INTERIM WITHDRAWL] WHICH
      *            OFFSETS AN 'ID' TRANSACTION.
      *            NEW PAYEE CODE 'IW' [INTERIM WITHDRAWL] OFFSETS AN 'ID' &
      *            PUTS CASH BACK TO THE DRAWER PR#790 WORLD
      *  CS 120221 ADDED WORLD-BANK-MESS
      * BAH 150713 CHANGED DIRECT-CHAN-TABLE TO ALLOW CHANGES TO FIELD 5
      *            THROUGH 8, ADR2 - ZIPCODE, REGMGM PR# 4786
      * KEC 2016.0204 CHANGED FMNO/CKFMNO FROM 3 TO 4 BYTES.
      * BAH 2016.0308 CHANGED TILLNO FROM PIC X TO PIC 9(2)
      * BAH 2016.0315 REPLACED SAVE-CX-REC WITH COPY REPLACING
      * BAH 2016.0615 ADDED BRNO TO HOLD-DT1-KEY, EXPANDED CLASS/DATE
      * BAH 2020.0110 COMMENTED OUT BRPRUFLAG-LIST, BRPRUFLAG 
      * BAH 2021.0209 ADDED DEBIT CARD TO THE TYPE-TABLE OCCURS 7, UMC
      *               ADDED DEBIT CARD TO THE PAYEE-TABLE OCCURS 18, UMC
      * BAH 2023.1027 ADDED VALUES TO HOLD FIELDS, CHECK ALREADY EXISTS BUG
      * BAH 2024.1203 REMOVED MILEAGE-FG #1691 S35Q-150
      ******************************************************************
       01  PR-IFN               PIC X(2) VALUE "PR".
       01  WK-IFN               PIC X(2) VALUE "WK".

       01  PROG-WK.
           03  FILLER      PIC X(7)  VALUE "LP/LONP".
           03  PROG-WK-IP  PIC XX.
       01  PRINT-PROG           PIC X(2) VALUE " ".

       01  MESSAGE-RESCISSION        PIC X(49)     VALUE
           "CHECKS CANNOT BE PRINTED WHILE LOAN IN RESCISSION".
       01  MESS-1.
           03  FILLER      PIC X(49) VALUE
           "F7-MENU, ^-BACK, M-MANUAL CHECK, S-SYSTEM CHECK:".

      * SYSTEM CHECK, F3-ALIGN - DEALER RSV WITHDRAWALS ALLOWED
       01  MESS-2A.
           03  FILLER      PIC X(38) VALUE
           "F7-MENU, F3-ALIGN, ^-BACK, F5/F6-SCAN,".
           03  FILLER      PIC X(22) VALUE
           " LN,MS,DL,DR,DC,DP,DD:".

      * MANUAL CHECK - DEALER RSV WITHDRAWALS ALLOWED
       01  MESS-2B.
           03  FILLER      PIC X(28) VALUE
           "F7-MENU, ^-BACK, F5/F6-SCAN,".
           03  FILLER      PIC X(22) VALUE
           " LN,MS,DL,DR,DC,DP,DD:".

      * SYSTEM CHECK, F3-ALIGN - DO NOT PERMIT DEALER RSV WITHDRAWALS
       01  MESS-2A1.
           03  FILLER      PIC X(38) VALUE
           "F7-MENU, F3-ALIGN, ^-BACK, F5/F6-SCAN,".
           03  FILLER      PIC X(16) VALUE
           " LN, MS, DL, DD:".

      * MANUAL CHECK - DO NOT PERMIT DEALER RSV WITHDRAWALS
       01  MESS-2B1.
           03  FILLER      PIC X(28) VALUE
           "F7-MENU, ^-BACK, F5/F6-SCAN,".
           03  FILLER      PIC X(16) VALUE
           " LN, MS, DL, DD:".

      *WORLD INTERIM DEPOSIT (ID) OR BK (BK)
       01  WORLD-BANK-DEPOSIT.
           03  FILLER      PIC X(28) VALUE
           "F7, ID(INTERIM DEPOSIT), BK ".
       01  WORLD-BANK-WITHDRAWAL.
           03  FILLER      PIC X(40) VALUE
           "F7, IW(INTERIM WITHDRAWAL), BK ".

       01  LONPI0-MESS-1.
           03  FILLER      PIC X(53) VALUE
           "F7-MENU, ^, C-CASH RECEIPTS, D-DEBIT CARD, R-RESERVE ".
           03  FILLER      PIC X(9)  VALUE
           "RECEIPTS:".
       01  DISBI0-MESS-1.
           03  FILLER      PIC X(60) VALUE
           "F7-MENU, ^, C-CASH DISBURSEMENT, D-DEBIT CARD DISBURSE:".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 61.
           03  LN-WK            PIC 999      VALUE 0.
           03  SAVE-LN-FEED     PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  BAL-FG              PIC 9(6) COMP-3.
       01  RSV-NEGATIVE        PIC X VALUE " ".
       01  RSV-EDIT            PIC ZZ,ZZZ,ZZ9.99-.
       01  W-GL.
           03  W-GLBR      PIC 9(4).
           03  W-GLNUM     PIC 9(7).
       01  W-GLNO REDEFINES W-GL PIC 9(11).

       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-DASH1   PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-DASH2   PIC X.
           03  ALP-BW6789  PIC X(4).
       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).

      * L-NUMBER1 = 99 (LOANS CANNOT USE THIS RANGE)
      * L-NUMBER2 = CX-SEQNO
      * L-SEQNO   = TT-CKWRITTEN SUBSCRIPT
       01  L-KEY.
           03  L-NUMBERX.
               05  L-NUMBER1  PIC 9(2).
               05  L-NUMBER2  PIC 9(4).
           03  L-NUMBER      REDEFINES L-NUMBERX
                              PIC 9(6).
           03  L-PAYEE-SUB    PIC 9(2).
       01  L-ACCTNO          REDEFINES L-KEY
                              PIC 9(8).
 
       01  TEST-ACCTNO        PIC 9(8).
       01  TEST-ACCTNOX REDEFINES TEST-ACCTNO.
           03  TEST-REF-ACCTNO PIC 9(6).
           03  TEST-REF-PAYEESUB PIC 99.

       COPY "LIBLP/LP01CX.CPY" REPLACING LEADING "CX" BY "SAVE-CX".

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-HEAD-BUF.
           03  TRANS-TYPE      PIC X.
           03  TRANS-DESC      PIC X(40).
           03  PAYEE-CODE      PIC XX.
           03  DLRVNO          PIC Z.
           03  PAYEE-DESC      PIC X(30).

       01  DISPLAY-HEAD-LIST.
           03  FILLER          PIC X(25) VALUE
           "K010 K011 K020 K021 K022.".

       01  DISPLAY-DEBIT-BUF.
           03  DEBIT-TRANS-TYPE      PIC X.
           03  DEBIT-TRANS-DESC      PIC X(40).
           03  DEBIT-PAYEE-CODE      PIC XX.
           03  DEBIT-DLRVNO          PIC Z.
           03  DEBIT-PAYEE-DESC      PIC X(30).


       01  DISPLAY-BUF         VALUE " ".
           03  D-010           PIC X(11).
           03  FILLER          REDEFINES D-010.
               05  D-0101      PIC Z(3)9.99.
               05  FILLER      PIC XXXX.
           03  FILLER          REDEFINES D-010.
               05  D-0102      PIC Z(5)9.
               05  FILLER      PIC XXXXX.
           03  D-020           PIC 9(2).
           03  D-021           PIC Z(6).
           03  D-CLDESC        PIC X(15).
           03  D-BWNAME        PIC X(18).
           03  D-030           PIC X(30).
           03  D-040           PIC X(25).
           03  D-050           PIC X(25).
           03  D-060           PIC X(15).
           03  D-070           PIC X(2).
           03  D-080           PIC X(10).
           03  D-090           PIC X(30).
           03  D-09A           PIC 99/99/99.
           03  D-09DESC        PIC X(9).
           03  D-09B-AREA.
               05  D-09B-BATCHID       PIC 9(10)         BLANK ZERO.
           03  D-09B-AREA2 REDEFINES D-09B-AREA.
               05  D-09B               PIC Z(6).99-      BLANK ZERO.
           03  D-09C           PIC Z(6).99-      BLANK ZERO.
           03  D-100           PIC Z,ZZZ,ZZ9.99- BLANK ZERO.
           03  D-110           PIC 9(6).
           03  D-111           PIC X(2).
           03  D-GLNO-GLAMT    OCCURS 5.
               05  D-GLBR      PIC ZZZ9.
               05  D-GLNUM     PIC ZZZZZZ9.
               05  D-GLAMT     PIC Z,ZZZ,ZZ9.99- BLANK ZERO.
           03  D-170           PIC X(70).
           03  D-180           PIC X(70).

       01  HOLD-DISPLAY-BUF.
           03  FILLER          PIC X(600).

       01  DISPLAY-LIST.
           03  FILLER          PIC X(46)  VALUE
           "010 020 021 CLDESC BWNAME 030 040 050 060 070 ".
           03  FILLER          PIC X(30)  VALUE
           " 080 090 09A 09DESC:S 09B 09C ".
           03  FILLER          PIC X(48)  VALUE
           "100 110 111 120 121 122 130 131 132 140 141 142 ".
           03  FILLER          PIC X(32)  VALUE
           "150 151 152 160 161 162 170 180.".
       01  DISPLAY-LIST2.
           03  FILLER          PIC X(54)  VALUE
           "010 020 021 CLDESC BWNAME 030 040 050 060 070 080 090.".

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  ONE             PIC S9.
       01  ELE             PIC 9(6) COMP-3.
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-TIME      PIC 9       VALUE 0.
       01  FIRST-SCAN      PIC X       VALUE "Y".
       01  FIRST-ELE       PIC 99      VALUE 0.

       01  MAX             PIC 99   VALUE 18.
       01  SPEC-TABLE.
      * 1  DL#/LN#/...
           03  FILLER      PIC X(7) VALUE "EN N090".
      * 2  CLASS
           03  FILLER      PIC X(7) VALUE "ENNN020".
      * 3  PAYEE NAME
           03  FILLER      PIC X(7) VALUE "E  A300".
      * 4  ADR 1
           03  FILLER      PIC X(7) VALUE "E  A250".
      * 5  ADR 2
           03  FILLER      PIC X(7) VALUE "E  A250".
      * 6  CITY
           03  FILLER      PIC X(7) VALUE "E  A150".
      * 7  STATE
           03  FILLER      PIC X(7) VALUE "E  A020".
      * 8  ZIP
           03  FILLER      PIC X(7) VALUE "E  A100".
      * 9  REASON
           03  FILLER      PIC X(7) VALUE "E  A300".
      *10  AMOUNT
           03  FILLER      PIC X(7) VALUE "EN S072".
      *11  REFERENCE #
           03  FILLER      PIC X(7) VALUE "EN N060".
      *12  GLNO 1
           03  FILLER      PIC X(7) VALUE "RN N040".
      *13  GLNO 2
           03  FILLER      PIC X(7) VALUE "RN N040".
      *14  GLNO 3
           03  FILLER      PIC X(7) VALUE "RN N040".
      *15  GLNO 4
           03  FILLER      PIC X(7) VALUE "RN N040".
      *16  GLNO 5
           03  FILLER      PIC X(7) VALUE "RN N040".
      *17  FREE TYPE 1
           03  FILLER      PIC X(7) VALUE "E  A700".
      *18  FREE TYPE 2
           03  FILLER      PIC X(7) VALUE "E  A700".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 18.
               05  SPEC    PIC X(7).
       01  CHFG-TABLE.
           03  CHFG        PIC X OCCURS 18.

      **************************************************
      *    CHANGE FLAG VALUE TABLE
      **************************************************
       01  CHFG-VAL-TAB.
      *---------------------- MANUAL CHECKS ---------------------------
      *(1) LN - LOAN
           03  FILLER      PIC X(18) VALUE "YNNNNNNNYYYYYYYYNN".
      *(2) DL - DEALER
           03  FILLER      PIC X(18) VALUE "YYYYYYYYYYYNNNNNNN".
      *(3) MS - MISCELLANEOUS
           03  FILLER      PIC X(18) VALUE "NNYNNNNNYYYYYYYYNN".
      *(4) VX - (DL INPUTED)
           03  FILLER      PIC X(18) VALUE "YYYYYYYYYYYNNNNNNN".

      *---------------------- SYSTEM CHECKS ---------------------------
      *(5) LN - LOAN
           03  FILLER      PIC X(18) VALUE "YNNNNNNNYYNYYYYYYY".
      *(6) DL - DEALER
           03  FILLER      PIC X(18) VALUE "YYYYYYYYYNYNNNNNYY".
      *(7) MS - MISCELLANEOUS
           03  FILLER      PIC X(18) VALUE "NNYYYYYYYYNYYYYYYY".
      *(8) VX - (DL INPUTED)
           03  FILLER      PIC X(18) VALUE "YYYYYYYYYYYNNNNNYY".

      *---------------------- OTHER CHECKS ---------------------------
      *(9) VOID (ALL PAYEE TYPES)
           03  FILLER      PIC X(18) VALUE "NNNNNNNNNNNNNNNNNN".
      *(10 DEALER CHECKS FROM RSV (DR, DP, DC)
           03  FILLER      PIC X(18) VALUE "YYYYYYYYYYYNNNNNYY".
      *---------------------- RESERVE SYSTEM -------------------------
      *(11 RESERVE DEALER CASH RECEIPTS (RR, RP, DC)
           03  FILLER      PIC X(18) VALUE "YYYYYYYYYYYNNNNNNN".
      *----------------------  CASH / BANK   -------------------------
      *(12 CASH (PAYEE NOT ENTERED)
           03  FILLER      PIC X(18) VALUE "NNYYYYYYYYNYYYYYYY".
      *(13 BANK (PAYEE, TILL NOT ENTERED)
           03  FILLER      PIC X(18) VALUE "NNYYYYYYYYYYYYYYYY".
      *(14 DEBIT CARD UMC, ONLY ENTER PAYEE, REASON, AMT AND GL
           03  FILLER      PIC X(18) VALUE "NNYNNNNNYYNYYYYYNN".
      *---------------------------------------------------------------
       01  CHFG-VAL-TABX  REDEFINES CHFG-VAL-TAB.
           03  CHFG-VAL    OCCURS 14
                           PIC X(18).

       01  CHFG-PTR-TAB.
      *(1) MANUAL
      * LN
           03  FILLER               PIC 99 VALUE 1.
      * DL
           03  FILLER               PIC 99 VALUE 2.
      * MISC
           03  FILLER               PIC 99 VALUE 3.
      * OVERPAY
           03  FILLER               PIC 99 VALUE 0.
      * NSF
           03  FILLER               PIC 99 VALUE 0.
      * DR
           03  FILLER               PIC 99 VALUE 10.
      * DC
           03  FILLER               PIC 99 VALUE 10.
      * DP (8)
           03  FILLER               PIC 99 VALUE 10.
      * VX (DL INPUTED)
           03  FILLER               PIC 99 VALUE 4.
      * RR
           03  FILLER               PIC 99 VALUE 11.
      * RC
           03  FILLER               PIC 99 VALUE 11.
      * RP
           03  FILLER               PIC 99 VALUE 11.
      * RD
           03  FILLER               PIC 99 VALUE 11.
      * CA
           03  FILLER               PIC 99 VALUE 12.
      * BK
           03  FILLER               PIC 99 VALUE 13.
      * CASH DRAWER DEPOSIT CHECK
           03  FILLER               PIC 99 VALUE 3.
      * INTERIM DEPOSIT
           03  FILLER               PIC 99 VALUE 3.
      * INTERIM WITHDRAWL 
           03  FILLER               PIC 99 VALUE 3.
      * (19) DEBIT CARD, UMC 
           03  FILLER               PIC 99 VALUE 3.

      *(2) SYSTEM
           03  FILLER               PIC 99 VALUE 5.
           03  FILLER               PIC 99 VALUE 6.
           03  FILLER               PIC 99 VALUE 7.
           03  FILLER               PIC 99 VALUE 0.
           03  FILLER               PIC 99 VALUE 0.
           03  FILLER               PIC 99 VALUE 10.
           03  FILLER               PIC 99 VALUE 10.
           03  FILLER               PIC 99 VALUE 10.
           03  FILLER               PIC 99 VALUE 8.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 13.
      * DD DRAWER DEPOSIT CHECK
           03  FILLER               PIC 99 VALUE 7.
      * INTERIM DEPOSIT
           03  FILLER               PIC 99 VALUE 7.
      * INTERIM WITHDRAWL
           03  FILLER               PIC 99 VALUE 7.
      * (19) DEBIT CARD, UMC
           03  FILLER               PIC 99 VALUE 7.

      *(3) VOID
           03  FILLER               PIC 99 VALUE 9.
           03  FILLER               PIC 99 VALUE 9.
           03  FILLER               PIC 99 VALUE 9.
           03  FILLER               PIC 99 VALUE 0.
           03  FILLER               PIC 99 VALUE 0.
           03  FILLER               PIC 99 VALUE 9.
           03  FILLER               PIC 99 VALUE 9.
           03  FILLER               PIC 99 VALUE 9.
           03  FILLER               PIC 99 VALUE 9.
           03  FILLER               PIC 99 VALUE 9.
           03  FILLER               PIC 99 VALUE 9.
           03  FILLER               PIC 99 VALUE 9.
           03  FILLER               PIC 99 VALUE 9.
           03  FILLER               PIC 99 VALUE 9.
           03  FILLER               PIC 99 VALUE 9.
           03  FILLER               PIC 99 VALUE 9.
      * INTERIM DEPOSIT
           03  FILLER               PIC 99 VALUE 9.
      * INTERIM WITHDRAWL
           03  FILLER               PIC 99 VALUE 9.
      * (19) DEBIT CARD, UMC
           03  FILLER               PIC 99 VALUE 9.

      *(4) DEALER RESERVE CASH RECEIPTS
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
           03  FILLER               PIC 99 VALUE 11.
      * DD DRAWER DEPOSIT CHECK
           03  FILLER               PIC 99 VALUE 11.
      * INTERIM DEPOSIT
           03  FILLER               PIC 99 VALUE 11.
      * INTERIM WITHDRAWL
           03  FILLER               PIC 99 VALUE 11.
      * (19) DEBIT CARD, UMC
           03  FILLER               PIC 99 VALUE 11.

      *(5) CASH
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 12.
           03  FILLER               PIC 99 VALUE 12.
      * DD DRAWER DEPOSIT CHECK
           03  FILLER               PIC 99 VALUE 12.
      * INTERIM DEPOSIT
           03  FILLER               PIC 99 VALUE 12.
      * INTERIM WITHDRAWL
           03  FILLER               PIC 99 VALUE 12.
      * (19) DEBIT CARD, UMC
           03  FILLER               PIC 99 VALUE 12.

      *(6) BANK
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
      * DD DRAWER DEPOSIT CHECK
           03  FILLER               PIC 99 VALUE 13.
      * INTERIM DEPOSIT
           03  FILLER               PIC 99 VALUE 13.
      * INTERIM WITHDRAWL
           03  FILLER               PIC 99 VALUE 13.
      * (19) DEBIT CARD, UMC
           03  FILLER               PIC 99 VALUE 13.

      *(7) DEBIT CARD
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
           03  FILLER               PIC 99 VALUE 13.
      * DD DRAWER DEPOSIT CHECK
           03  FILLER               PIC 99 VALUE 13.
      * INTERIM DEPOSIT
           03  FILLER               PIC 99 VALUE 13.
      * INTERIM WITHDRAWL
           03  FILLER               PIC 99 VALUE 13.
      * (19) DEBIT CARD, UMC, USE CHFG-PTR(14) FOR CHFG-TABLE, FIELDS TO ENTER
           03  FILLER               PIC 99 VALUE 14.

       01  CHFG-PTR-TABX           REDEFINES CHFG-PTR-TAB.
           03  CHFG-TRANS-TYPE      OCCURS 7.
               05  CHFG-PAYEE-TYPE  OCCURS 19.
                   07  CHFG-PTR     PIC 99.

      *    EXCEPTION TABLE FOR DIRECT LOANS AND TRANS POSTING
       01  SALES-CHAN-TABLE   PIC X(18) VALUE "NNYYYYYYNNNNNNNNYY".
       01  DIRECT-CHAN-TABLE  PIC X(18) VALUE "NNYYYYYYNNNNNNNNYY".
       01  TRANS-CHAN-TABLE   PIC X(18) VALUE "NNYYYYYYYNNYYYYYYY".
       01  DIRECT-NOSCAN      PIC X(18) VALUE "YYYYYYYYYYYYYYYYNN".

      *    ____________________________________________
      *    [     P  A  Y  E  E      T  A  B  L  E      ]
      *    [     CX-TYPE                               ]
      *    --------------------------------------------
      *    MAINTAIN TT-CKWRITTEN ORDER WHEN ADDING CODES.
       01  PAYEE-TABLE.
      *     CD<----PAYEE---->
           03  FILLER           PIC X(32) VALUE
           "LNLOAN PROCEEDS CHECK           ".
           03  FILLER           PIC X(32) VALUE
           "DLDEALER PROCEEDS CHECK         ".
           03  FILLER           PIC X(32) VALUE
           "MSMISCELLANEOUS CHECK           ".
           03  FILLER           PIC X(32) VALUE
           "  OVERPAYMENT                   ".
           03  FILLER           PIC X(32) VALUE
           "  NSF                           ".
      *  TT-CKWRITTEN (6) IS WITHDRAWALS FROM BANK BUT NOT A PAYEE
           03  FILLER           PIC X(32) VALUE
           "DRDEALER RESERVE FROM PRINCIPAL ".
           03  FILLER           PIC X(32) VALUE
           "DCDEALER RESERVE FROM CHARGES   ".
           03  FILLER           PIC X(32) VALUE
           "DPDEALER PARTICIPATION HELD     ".
           03  FILLER           PIC X(32) VALUE
           "VXDEALER PROCEEDS CHECK (INPUT) ".
           03  FILLER           PIC X(32) VALUE
           "RRRECEIPT TO PRINCIPAL RESERVE  ".
           03  FILLER           PIC X(32) VALUE
           "RCRECEIPT TO CHARGES RESERVE    ".
           03  FILLER           PIC X(32) VALUE
           "RPRECEIPT TO PARTICIP.  RESERVE ".
           03  FILLER           PIC X(32) VALUE
           "RDRETURN OF DEALER PROCEEDS     ".
           03  FILLER           PIC X(32) VALUE
           "CACASH DRAWER                   ".
           03  FILLER           PIC X(32) VALUE
           "BKBANK WITHDRAWAL               ".
           03  FILLER           PIC X(32) VALUE
           "DDDRAWER DEPOSIT CHECK          ".
           03  FILLER           PIC X(32) VALUE
           "IDINTERIM DEPOSIT               ".
      * REVERSE INTERIM DEPOSIT
           03  FILLER           PIC X(32) VALUE
           "IWINTERIM WITHDRAW              ".
           03  FILLER           PIC X(32) VALUE
           "DBDEBIT CARD TRANSACTION        ".

       01  PAYEE-TABLEX         REDEFINES PAYEE-TABLE.
           03  PAYEE-CD-DESC-CHFG OCCURS 19.
               05  P-CODE       PIC X(2).
               05  P-DESC       PIC X(30).

       01  PAYEE-MAX            PIC 99   VALUE 19.
       01  PAYEE-SUB            PIC 99   COMP.
       01  CHECK-TRCD           PIC XX.
           88  DEALER-ORIGINAL  VALUE "NW" "PU" "TX".
       01  CHECK-PAYEE          PIC XX.
           88  DEALER           VALUE
           "DL" "DR" "DC" "DP" "VX" "RR" "RC" "RP" "RD".
           88  DEALER-RSV       VALUE "DR" "DC" "DP".
           88  DEALER-RSV-REC   VALUE "RR" "RC" "RP".
           88  DEALER-PROC-REC  VALUE "RD".
           88  DEALER-PROCEEDS  VALUE "DL" "VX".
           88  DEALER-INPUT     VALUE "VX".
           88  LOANCK           VALUE "LN".
           88  MISCCK           VALUE "MS".
           88  BANKTR           VALUE "BK".
           88  CASHTR           VALUE "CA".
           88  DRAWCK           VALUE "DD".
      * INTERIM DEPOSIT
           88  INTERDEP         VALUE "ID".
           88  INTERWITH        VALUE "IW".
      *    ____________________________________________
      *    [     T  Y  P  E         T  A  B  L  E      ]
      *    [     CX-PAID,   TRANS TYPE                 ]
      *    --------------------------------------------
       01  TYPE-TABLE.
           03  FILLER           PIC X(26) VALUE
           "MMANUAL CHECK (TYPED)     ".
           03  FILLER           PIC X(26) VALUE
           "SSYSTEM CHECK (PRINTED)   ".
           03  FILLER           PIC X(26) VALUE
           "VVOID CHECK               ".
           03  FILLER           PIC X(26) VALUE
           "RDEALER RSV CASH RECEIPT  ".
           03  FILLER           PIC X(26) VALUE
           "CCASH RECEIPT/DISBURSE    ".
           03  FILLER           PIC X(26) VALUE
           "BBANK TRANSACTION         ".
           03  FILLER           PIC X(26) VALUE
           "DDEBIT CARD TRANSACTION   ".
       01  TYPE-TABLEX         REDEFINES TYPE-TABLE.
           03  TYPE-CD-DESC-CHFG OCCURS 7.
               05  T-CODE       PIC X(1).
               05  T-DESC       PIC X(25).
       01  TYPE-MAX             PIC 9(2) VALUE 7.
       01  TYPE-SUB             PIC 9(2).

      *-----------------------------------------------------------
      *    WORKERS REQUIRED FOR COPY MEMBER LIBLP/CKPRINTUPD
      *-----------------------------------------------------------
       01  EDIT-FM-LIST         PIC X(6)    VALUE "FMKEY.".

       01  INSPECT-W           PIC X(34).
       01  ADD-ON              PIC X(16).
       01  SUB                 PIC 9(6).
       01  SUB1                PIC 9(6).

       01  CHECK-NO            PIC 9(6).
       01  CHECK-NO2           PIC Z(5)9.
       01  TILLNO              PIC 99.
       01  HOLD-BRNO           PIC 9(4)        VALUE 0.
       01  HOLD-CHECK          PIC 9(6)        VALUE 0.
       01  HOLD-DLRVNO         PIC 9           VALUE 0.
       01  HOLD-TRANS-TYPE     PIC X           VALUE SPACES.
       01  HOLD-CHECK-PAYEE    PIC XX          VALUE SPACES.
       01  HOLD-BATCHID-SEQ    PIC S9(5)  COMP VALUE 0.
       01  HOLD-CX-PAID        PIC X           VALUE SPACES.
       01  HOLD-CX-REASON      PIC X(30)       VALUE SPACES.
       01  HOLD-DT-TRCD        PIC XX          VALUE SPACES.
       01  HOLD-SEQ            PIC 9(6)        VALUE 0.
       01  HOLD-DT1-KEY.
           03  HOLD-DT-BRNO              PIC 9(4).
           03  HOLD-DT-DLNO              PIC 9(4)V99.
           03  HOLD-DT-CLASS             PIC 9(3).
           03  HOLD-DT-ACCTNO            PIC 9(8).
           03  HOLD-DT-DATE              PIC 9(8).
           03  HOLD-DT-SEQ               PIC 9(3).

      *  GLB  07/10/02  NEED TO TEST ONLY 4 DIGITS
      *       OF DEALER MASTER WHEN MULTI BRANCH CHECKS
       01  HOLD-CX-NUM           PIC 9(6)        VALUE 0.
       01  FILLER REDEFINES HOLD-CX-NUM.
              03  HOLD-CX-DL     PIC 9(4).
              03  HOLD-CX-DLBR   PIC 9(2).
       01  HOLD-CX-DLNO REDEFINES HOLD-CX-NUM PIC 9(4)V99.

       01  FIRST-READ           PIC X VALUE "Y".
       01  FIRST-CHECK          PIC X VALUE "Y".
       01  REC-EXISTS           PIC X.
       01  NEW-CX-PAID          PIC X.
