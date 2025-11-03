      * COPYMEMBER: LIBGB/GB01BR
      **************************************************************************
      *    BRANCH FILE RECORD      (GB01BR)   
      *
      *REV:
      * JTG 033095 ADDED BR-UNITPER-FG AND BR-UNITPER-Y-D-FG
      * JTG 051595 ADDED BR-RI-BEFORE-REPO
      *                  BR-RI-BEFORE-P4P5
      * JTG 061595 ADDED BR-NOT-OURS-REFUND
      * JTG 061995 ADDED BR-SFDLO3-WINDOW-FG
      * JTG 061995 ADDED BR-CALC-INS-ADJ
      * JTG 051595 ADDED BR-AC-BEFORE-REPO
      *                  BR-AC-BEFORE-P4P5
      * MJD 063095 ADDED BR-BANK-CLEARING-GL
      *                  BR-BANK-ROUTING-NO
      *                  BR-BANK-ACCTNO.
      * JTG 070595 ADDED BR-REB-METHOD-2       OCCURS 5
      *                  BR-REBDAYS-MONTH-2    OCCURS 5
      *                  BR-ACCRUAL-MONTHS-2   OCCURS 5
      * JTG 071395 ADDED BR-DEFAULT-SC-SUBCLASS
      * BLL 071795 EXPANDED UNUSED, BANK ACCT, ADDED BANKGL-DEPOSIT
      * BAH 083195 REMOVED BR-CASH-METHOD-ONLY AND BR-IB-ACCRUAL
      *            (MOVED TO GP RECORD)
      * MJD 011296 ADDED BR-NO-OUT-DLR-RSV.
      * KEC 020796 ADDED BR-COLLECTOR-QUEUE AND BR-COLL-QUEUE-CON-REC.
      * JTG 022096 ADDED BR-LNCHGS-WINDOW
      * JTG 062596 ADDED BR-EXPAND-APP-INPUT
      * BAH 082696 ADDED BR-A50-CPCORP TO USE A50'S COLLECTION REPORT (INSTANT)
      * MJD 100396 ADDED BR-INBR-BILNSC-FG. IF 'Y' THEN GLOBAL SCAN IS
      *            CALLED, ELSE A SINGLE BRANCH IS ENTERED.
      * KEC 112096 ADDED BR-LEDGER-JUST-INTCHG FOR QUICK CREDIT,
      *             FOR "MQ" AND "MR" TO ONLY PRINT INTCHG IN THE
      *             FINANCE CHARGE BOX.  FOR COPYING LP/LONPMQ FROM A48.
      * MJD 012897 ADDED BR-BYINQ-START-PAGE AND BR-DSINQ-START-PAGE.
      *            THESE ARE THE DEFAULT START PAGES FOR INQUIRIES.
      * JTG 022097 ADDED BR-APP-REQUIRED-FOR-INPUT  RE: STATISTICS UPDATE
      * BLV 022597 ADDED BR-CREDIT-SCORING-FG
      * BLV 041497 ADDED BR-COL-QUEUE-LOW-TO-HI
      * MJD 071797 ADDED BR-CAN-GROUP-ID.
      * KEC 072297 ADDED BR-CREDIT-BUREAU-SERVICES (5 BYTES).
      * MJD 072397 ADDED BR-AUTO-LNSTAT-WINDOW.
      * BLV 072497 ADDED BR-TIME-ZONE-FACTOR.
      * MJD 072897 ADDED BR-LONPF-OPT-DISPLAY.
      * JTG 081397 ADEED BR-DUEDATE-CHANGE-CD RELIANCE, #63
      * KEC 082097 ***CONVERSION-A90-TO-A10***
      *               ADDED BR-GIFTNET, BR-GIFTNET-LEDGER-LINE.
      * KEC 082697 REMOVED BR-DL-CHECK-NAME-CHG PER BETH BECAUSE OF PR#0543;
      *             CHANGING OF PAYEE IS ALREADY ALLOWING, SO THIS FLAG IS
      *             NOT NECESSARY.  MADE INTO FILLER.
      * MJD 082797 ADDED BR-NO-FT-EMP AND BR-NO-PT-EMP FOR BYMISC
      * MJD 082797 ADDED BR-NO-FT-EMP AND BR-NO-PT-EMP FOR MN/BYMISC.C
      * JTG 092597 ADDED BR-BALLOON-LOANS-FG, CASTLE FINANCE
      * BLV 103097 ADDED BR-GL-ENTRY-DEFAULT, BR-GL-TYPE-ALLOWED
      * BLV 110497 ADDED BR-MIDSTREAM-1STPAY-FG
      * JTG 111197 ADDED BR-DUEDATE-CHANGE-CD OPTION 'M' MERCURY
      * JTG 111197 ADDED BR-NODEF-1ST-X-MONTHS OPTION MERCURY
      * JTG 112197 EXPANDED RECORD BY 52 BYTES 798 --> 850
      * JTG 120197 ADDED WORLD BR-ADD-INS-FEECODE AD&D INSURANCE GA
      *                        BR-ROAD-G-SGL-FEECODE FOR VOYAGER
      *                        BR-ROAD-G-FAM-FEECODE
      * JTG 120397 ADDED BR-NODEF-IF-ZERO-AMT OPTION MERCURY
      * KEC 121797 ADDED BR-FXPRESS-OPTION FOR FORMSXPRESS FORMS.
      * MJD 012898 ADDED BR-NEXT-SOLICIT
      * BAH 022698 ADDED BR-ADDON-FEECD-O3 PIC XX OCCURS 2
      * BAH 022698 REMOVED BR-MEDIA-PURGE
      * MJD 031398 ADDED BR-SIZE - 78 LEVEL DOES NOT AFFECT REC SIZE.
      * BAH 032498 ADDED BR-IMMEDIATE-RECEIPT AND BR-IMMEDIATE-DOC-PRINT
      *            FROM A90 FOR WORLD...
      * BAH 042998 CHANGED BR-ADDON-FEECD TO BR-ADDON-FEECD-O2, BECAUSE WE
      *            DONT ALLOW FEES ON OTHER 1 ANYMORE..
      * BLV 052298 ADDED BR-CHECK-EOM-FG: CHECK EOM & CALENDAR EOY
      *            COMPLETE BEFORE ALLOWING LONPA0 TO OPEN DAY
      * BAH 060298 ADDED BR-AUTO-PL-REF-AC FOR MERCURY #E180 TO AUTOMATICALLY
      *            REFUND EVERYTHING AND ACCELERATE THE ACCOUNT
      * BLV 090298 TRASHED BR-EOM1-PROCEDURE, BR-EOM2-PROCEDURE, THESE ARE
      *            NOT USED & ARE CONFUSING PEOPLE
      * KEC 100698 ADDED BR-REVOLVING & BR-FAX-NUMBER. REGENCY REVOVLING
      * JTG 101298 CHANGED BR-REVOLVING TO BR-REVOLVING-SF-FG
      *                          AND ADDED BR-REVOLVING-LN-FG
      * CS  111798 ADDED BR-ALPHA-DRAWERS
      * KEC 011599 ADDED BR-REVOLVING-CONV-DATE SO TO PREVENT ANY
      *             UPDATES TO 'GL' FOR MANUAL CONVERSION OF REVOLVING
      *             ACCOUNTS (REGENCY).
      * MJD 990125 ----- UPDATED IN FLEXGEN -----
      * CS  1999.0312 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * GLF 990323 ADDED BR-LATECHG-ON-ALLOTMENTS, TFC #018
      * MJD 990323 ADDED BR-PRINTERFG-VALID. 88 LEVEL FOR VALID PRINTER
      *            CODES.
      * MJD 990412 CHANGED BR-BP-NEXT-BATCH TO BE PIC 9(2) COMP-3
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * MJD 990617 ADDED BR-ALLOW-SYS-AS-EFFDATE (REG MGMT REQ # 867)
      * GLF 990715 ADDED BR-TAX-PREBOOK-CUST AND BR-TAX-PREBOOK-NONCUST
      *            WORLD PR#211
      * GLB 990715 ADD BR-PASSWORD-DRAWER FLAG, PR #901, REGMGM
      * BAH 990723 REMOVED BR-FXPRESS-OPTION
      * KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * MJD 990901 ADDED BR-INBR-BILNSC-FG-VALID - FLAG NOW CAN HAVE
      *            VALUE OF "M" (MUL PR# 925)
      * JTG 991129 ADDED BR-ONESTEP-RN-FG, TO ALLOW ONE STEP RENEWALS, WORLD
      * KEC 991130 ADDED BR-LI-LISCAN-FG; FOR CMSI/CSI PROJECT
      * MJD 000223 ADDED BR-HIDE-END-BAL (WORLD PR#219)
      * KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * MJD 000420 ADDED A SECOND SET OF BATCH ALLOTMENT FLAGS FOR LOCK
      *            BOX PROCESSING (MERC PR#227)
      * BAH 000531 ADDED NEW VALUE FOR BR-NO-CASH-PROCEEDS "B" TO ALLOW
      *            CASH PROCEEDS ON PB ACCTS ONLY, SUNBELT PR# 1213
      * MJD 000911 ADDED BR-LBOX AND BR-LATECHG-ON-LBOX (MFC PR#227)
      * JTG 001117 ADDED BR-ALLOW-OV-TRCD FOR 'OV' OVERPAYMENT TRANSACTION
      *                                                            PR#J015
      * MJD 001120 ADDED BR-GLOBAL-QUEUE (TFC PR#60)
      * JTG 001218 ADDED THE FOLLOWING FIELDS, RE: GP-ALLOW-SET-FOR-NONCASH
      *            MERCURY PR#239
      *            03  BR-BP-PAYOFF-NONCASH      PIC X.
      *            03  BR-BP-PAYOFF-OV           PIC X.
      *            03  BR-LBOX-PAYOFF-NONCASH    PIC X.
      *            03  BR-LBOX-PAYOFF-OV         PIC X.
      * BLV 010604 ADDED BR-ALLOW-INTERBR - IF "N" DON'T ALLOW OTHER
      *            BRANCHES TO POST INTERBRANCH PAYMENTS TO THIS BRANCH,
      *            REGACC PR# 1510
      * BAH 010621 NO PHYSICAL CHANGE, JUST CHANGED THE USE OF REB-METHOD-2
      *            SUBSCRIBT 4 FROM O6 TO CL(LEV), THE LOAN RECORD DOES NOT
      *            EVEN ALLOW O6, REGWAN PR# 1500
      * BLV 011207 ADDED BR-COLL-QUEUE-PAST-DUE, REGACC PR# 1677
      * JTG 020208 ADDED BR-FORCE-VALID-CITY-COUNTY                 WORLD PR#288
      * CS  020926 ADDED BR-LI-DLCOMM-CMSI-PP-MAX, BR-LI-DLCOMM-CMSI-O3-MAX..FOR
      *            MAXIUMUM AMOUNT OF DEALER COMMISSION FOR PP & O3 INSURANCE
      *            FED FROM CSMI(LOAN APP INTERFACE UPD) TO LOAN MANAGER
      * BAH 030507 ADDED BR-ASSESS-LATE-CHGS FOR MODERN PR# 2036, THEY WILL
      *            SET THIS TO "N", THEY DONT WANT LATE CHARGES IN LEGAL BRANCH
      * BAH 030917 REMOVED BR-FAIRCO-CPCORP-FG, BR-LNCHGS-WINDOW,
      *            BR-FAIRCO-TRW-OPT
      * BAH 030918 REMOVED BR-AS-TRAILER-NUMBER,MADE FILLER X(5),
      *            NOT USED ANYWHERE
      * KEC 030918 {PR#00039} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED   03  BR-LR-GROUP-COLLID       PIC X.
      * BLV 031112 ADDED BR-INS-PRODUCER, WORLD NEEDS THIS FIELD IN BRANCH,
      *            NOT GLOBAL FOR CLAIMS; REDEFINED BR-STMNT2 SINCE ALMOST
      *            OUT OF SPACE
      * BLV 040218 ADDED BR-JE-ALLOCATION-SKIP, FLAG TO CAUSE GL JOURNAL ENTRY
      *            ALLOCATIONS TO SKIP BRANCH, WORLD
      * JTG 040219 ALLOWED NON MONTHLY CAN LOANS (NMQUOT.C) WORLD J043
      *             ADDED BR-NON-MONTHLY-CAN-LOANS
      * BLV 040315 RENAME BR-JE-ALLOCATION-SKIP TO BR-WORLD-BRANCH-TYPE, WILL
      *            BE USED BY WORLD TO INDICATE STATE ADMIN BRANCHES, 1ST &
      *            2ND YEAR DENOVO'S
      * BLV 040323 OKAY, NEED BOTH BR-JE-ALLOCATION-SKIP & WORLD-BRANCH-TYPE,
      *            SO RENAMED THE ALLOCATION FLAG BACK & ADDED THE TYPE, WORLD
      * BLV 040615 ADDED BR-COL-QUEUE-BEHAVIOR-FG, WHETHER TO ORDER QUEUE
      *            BY BEHAVIOR GRADE, REGACC PR# 59
      * JTG 040826 ALLOW BATCH PAYMENTS TO ACCOUNTS WITH OTHER BALANCE BUT
      *            NOT APPLY APPLY MONIES TO OTHER BALANCE          REGACC #0070
      * BAH 040830 ADDED BR-REGION-NUMBER FOR REGACC PR# 76
      * BAH 150930 CHANGED FROM RELATIVE TO INDEXED, PD0005
      * BAH 151020 COMBINED WITH B2FILE, CHANGED COMP-6 TO COMP-3, ADDED
      *            FILLER, INCREASED BR-OPEN-BAL TO OCCURS 20, REMOVED
      *            BR-REVOLVING-CONV-DATE, MADE BR-INS-PRODUCER ITS OWN
      *            FIELD INSTEAD OF REDEFINE OF STMNT2, INCREASED
      *            BR-REB-METHOD AND METHOD-2 TO 99, PD0003
      *  CS 160120 REMOVED BR-REB-METHOD-2 FIELDS
      * BLM 160914 ADD BR-ALLOW-SCANNED-CKS, WORLD PR# 1188
      * BLM 160915 ADD BR-CHECK-DEPOSIT-GL, WORLD PR# 1188 
      * AMM 161031 ADD BR-OPTION-PST-LASER, WORLD PR# 1200
      * BAH 180214 ADDED BR-USE-DEBIT-CARDS, BR-GL-DEBIT-CARD-PROCEEDS
      *            FOR NETSPEND, WORLD PR#1306
      * AMM 181113 REMOVED CREDIT BUREAU SERVCES FIELDS FORM RECORD LAYOUT
      *            SDP_002_T02
      *  CS 190124 ADD BR-WKDAY-BANKID FOR BANK INTEGRATION WORKDAY PR#1353A
      *  CS 190408 ADD FUTURE FIELDS,SAVED GB01BR.190408
      *  BAH 20190625 REMOVED BR-REVOLVING-SF-FG BR-REVOLVING-LN-FG PT0153
      *  BAH 20190702 REMOVED BR-USE-PARTIC-FLAT-TABLE PT0051
      *  BAH 20190730 ADDED BR-PURGE-NOW-FLAG TO REPLACE THE "PURGEME" TAG
      *               IN LNARCH. LNARCH GETS PUT IN A BATCH AND ONLY THE
      *               BRANCHES WITH THIS FLAG SET WILL ACTUALLY PURGE
      *  BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *  BAH 20200326 ADDED BR-EMAIL AND BR-MANAGER-EMAIL
      *  BAH 20210421 REMOVED FUTURE FIEKDS. MOVE BR-NO TO BE GROUP LEVEL
      *  BAH 20220921 ADDED BR-VERITEC-LICENSE FOR "IL" VERITEC EXTRACTS #1570
      *  BAH 20230419 ADDED BR-TEST-PP-AUTO-INSCOVR #1594A
      *  20230600 BAH ADDED FILLER
      *  20230713 BAH ADDED BR-USE-NEW-WFARGO-ACCT #1616
      *  20230727 BAH ADDED BR-BRANCH-CLOSED #1618
      *  20231110 BAH ADDED BR-ALLOW-F3-ALL-DOCS FOR DOCUMENT PRINT #1630
      **************************************************************************
       78  BR-SIZE                      VALUE 1615.
       01  BR-REC.
           03  BR-KEY.
               05  BR-NO                PIC 9(4).
      $XFD DATE=YYYYMMDD
           03  BR-LTOUCH-DATE           PIC 9(8)     COMP-3.
           03  BR-NAME                  PIC X(40).
           03  BR-ADR1                  PIC X(24).
           03  BR-ADR2                  PIC X(24).
           03  BR-CITY                  PIC X(15).
           03  BR-STATE                 PIC XX.
           03  BR-ZIP                   PIC X(10).
           03  BR-PHONE                 PIC 9(10)    COMP-3.
           03  BR-RETAIN                PIC 9(11)    COMP-3.
           03  BR-NEXT-CHECK            PIC 9(6)     COMP-3.
           03  BR-PRINTERS.
               05  BR-PRINTTAB         OCCURS 4.
                   07  BR-PRINTER       PIC 99.
           03  BR-FEE-GL-BRNO           PIC 9(4).
           03  BR-REMIT1                PIC X(30).
           03  BR-REMIT2                PIC X(30).
           03  BR-REMIT3                PIC X(30).
           03  BR-STMNT1                PIC X(60).
           03  BR-STMNT2                PIC X(60).
           03  BR-INS-PRODUCER-NUM      PIC 9(8).
           03  BR-DLRCHK                PIC X.
           03  BR-FRONT-END-FG          PIC X.
           03  BR-EMLNRG-PHONE          PIC X.
           03  BR-POST-GL               PIC X.
           03  BR-ALLOTMNTS             PIC X.
           03  BR-EXTRACT-AUTO-UPD      PIC X.
           03  BR-PCNI-LEDGER           PIC X.
           03  BR-PRINTERFGS.
               05  BR-PRINTFGTAB       OCCURS 4.
                   07  BR-PRINTERFG     PIC X.
                       88 BR-PRINTERFG-CENTRONICS   VALUE "C".
                       88 BR-PRINTERFG-NEC          VALUE "N".
                       88 BR-PRINTERFG-OKIDATA      VALUE "O".
                       88 BR-PRINTERFG-OKIDATA-IBM  VALUE "I".
                       88 BR-PRINTERFG-LASER        VALUE "L".
                       88 BR-PRINTERFG-LASER-IIP    VALUE "2".
                       88 BR-PRINTERFG-PANASONIC    VALUE "P".
                       88 BR-PRINTERFG-DEFAULT      VALUE " ".
                       88 BR-PRINTERFG-VALID VALUE "C" "N" "O" "L"
                                                   "2" "P" " " "I".
           03  BR-NEGDLRSV              PIC X.
           03  BR-CPCORP-ORDER          PIC X.
           03  BR-PRIO-MEMO-FLAG        PIC X.
           03  BR-PREBILL-PL-JD-OPT     PIC X.
           03  BR-LOAN-NUM-FLAG         PIC X.
           03  BR-DOWNPY-FLAG           PIC X.
           03  BR-AUTO-INS-EARN         PIC X.
           03  BR-OTH-INS-LEVELS        PIC X.
           03  BR-ALLOW-INTERBR         PIC X.
           03  BR-UNER-DLANRP           PIC X.
           03  BR-SECOND-PROMISE        PIC X.
           03  BR-AVAIL-CREDIT          PIC X.
           03  BR-RECP1                 PIC X(30).
           03  BR-RECP2                 PIC X(30).
           03  BR-INC-WAIVERS           PIC X.
           03  BR-INC-INS               PIC X.

      *     BATCH PAYMENT FIELDS:
           03  BR-BP-NEXT-BATCH         PIC 9(2). 
           03  BR-BP-NOPAYOFF           PIC X.
           03  BR-BP-ODDPAY             PIC X.
           03  BR-BP-LC                 PIC X.
           03  BR-BP-NOPRIN             PIC X.
           03  BR-BP-FLASH              PIC X.
           03  BR-BP-ACTIONCD           PIC X.
           03  BR-BP-MAXCON             PIC 9(3)     COMP-3.
           03  BR-BP-MAXREC             PIC 9(3)     COMP-3.
           03  BR-BP-PL                 PIC X.
           03  BR-BP-JD                 PIC X.
           03  BR-BP-BRANCH             PIC 9(5)     COMP-3.

           03  BR-AGE-ERN               PIC X.
           03  BR-OTHER-BRANCH-GL       PIC X.
           03  BR-SALES-SMALL           PIC X.
           03  BR-NEW-COLL-INQUIRY      PIC X.
           03  BR-CHANGE-MEMO           PIC X.
           03  BR-INS-ADDON             PIC X.
               88  BR-INS-ADDON-VALID  VALUE "O" "Y" "N".
               88  BR-INS-ADDON-ORIG   VALUE "O".
               88  BR-INS-ADDON-NEW    VALUE "Y".
           03  BR-TRAILER-REC           PIC X.
           03  BR-ALDEL-FLAG            PIC X.
           03  BR-IB-LCHG-MESS          PIC X.
           03  BR-COL-QUEUE-PAST-DUE    PIC X.
           03  BR-LI-DLCOMM-CMSI-PP-MAX PIC 9(4)V99 COMP-3.
           03  BR-ADDON-FEECD-O2        PIC XX OCCURS 2 TIMES.
           03  BR-AUTO-CHECK            PIC X.
           03  BR-RECEIPT-PROMDATE      PIC X.
           03  BR-RECEIPT-INTPD         PIC X.
           03  BR-COLL-ID-SCAN          PIC X.
           03  BR-STOP-ON-COLL          PIC X.
           03  BR-DUEDATE-CHANGE-CD     PIC X.
           03  BR-CHECK-EOM-FG          PIC X.
           03  BR-VOLUME-DLANRP         PIC X.
           03  BR-PDTH-NXDUE            PIC X.
           03  BR-LC-CODE               PIC X.
           03  BR-VALIDATE-PRUS         PIC X.
           03  BR-AUTO-OPEN-BAL         PIC X.
           03  BR-OPEN-BALS.
               05  BR-OPEN-BAL          PIC S9(7)V99 COMP-3 OCCURS 20.
           03  BR-TENDER-CD             PIC X(5)     OCCURS 3.
           03  BR-DLPARV-ADJ-FG         PIC X.

           03  BR-OVERRIDE-BR-STATE     PIC X.
           03  BR-DEFAULT-SC-CLASS      PIC 9(3).
           03  BR-RECDEL-NOT-REC        PIC X.
           03  BR-POST-AHEAD-FG         PIC X.
           03  BR-CAN-LOANS-FG          PIC X.
               88  BR-CAN-LOANS-YES                  VALUE "Y".
               88  BR-CAN-LOANS-NO                   VALUE "N".
               88  BR-CAN-LOANS-ONLY                 VALUE "O".
               88  BR-CAN-LOANS-VALID                VALUE "Y"
                                                           "N" "O".
           03  BR-NON-MONTHLY-CAN-LOANS      PIC X.
           03  BR-BP-ALLOW-PMT-ACCT-OTHBAL   PIC X.
           03  BR-LBOX-ALLOW-PMT-ACCT-OTHBAL PIC X.
           03  BR-REGION-NUMBER              PIC 9(2).
           03  BR-OUR-FEE                    PIC X.

      *=================================================================
      *   THIS IS HOW IT IS USED IN A15.
      *    CL(DEC)  -  1      O1      - 4
      *    AH       -  2      O2      - 5
      *    PP       -  3
      *---------------------------------------------------------------
      *    ONLY METHOD2(1) AND METHOD2(4) ARE USED
      *    O3       -  1      CL(LEV) - 4
      *    O4       -  2      N/A     - 5
      *    O5       -  3
      * THE LOAN RECORD ONLY HAS O1 - O5, SO I USED O6 FOR CREDIT LIFE
      * LEVEL, REGWAN WANTS CREDIT LIFE DEC AND CREDIT LIFE LEV
      *---------------------------------------------------------------
      *   IN A30 
      *    CL(DEC)  -  1      O2       - 5     
      *    AH       -  2      O3       - 6     O5     -  9 
      *    PP       -  3      CL(LEV)  - 7     O6     - 10      
      *    O1       -  4      O4       - 8     O7     - 11
      *---------------------------------------------------------------

           03  BR-REB-METHOD            PIC 99       OCCURS 11.
           03  BR-REBDAYS-MONTH         PIC 99       OCCURS 11.
           03  BR-ACCRUAL-MONTHS        PIC 999      COMP-3 OCCURS 11.

      *=================================================================
           03  BR-GLOVERSHORT           PIC 9(11)    COMP-3.
           03  BR-GLBANK                PIC 9(11)    COMP-3.
           03  BR-GLONHAND              PIC 9(11)    COMP-3.
           03  BR-BANKBAL               PIC S9(9)V99 COMP-3.
           03  BR-RETAIN-AMT            PIC S9(9)V99 COMP-3.
           03  BR-LCARDFG               PIC X.

           03  BR-DATA-PATH             PIC X(6).
           03  BR-MACHINE               PIC XX.
           03  BR-MANAGER               PIC X(25).

           03  BR-EOD-PROCEDURE         PIC XXX.
           03  BR-BP-PAYOFF-NONCASH     PIC X.
           03  BR-BP-PAYOFF-OV          PIC X.
           03  BR-LI-DLCOMM-CMSI-O3-MAX PIC 9(4)V99 COMP-3.
           03  BR-LR-GROUP-COLLID       PIC X.

           03  BR-UNITPER-FG            PIC X.
           03  BR-UNITPER-Y-D-FG        PIC X.

           03  BR-NO-PROMPT-FOR-OTHBR    PIC X.
           03  BR-WORLD-BRANCH-TYPE      PIC X.
           03  BR-COL-QUEUE-BEHAVIOR-FG  PIC X.
           03  BR-CONVERT-AUTO-ACCTNO    PIC X.
           03  BR-TS-REPO-INS-FG         PIC X.
           03  BR-BOOK-SF-BR-ONLY        PIC X.
           03  BR-FORCE-VALID-CITY-COUNTY PIC X.
           03  BR-SET-COLLCD-DL-MFGCD    PIC X.
           03  BR-DEFAULT-SF-UNITPER-CD  PIC X.
           03  BR-DEFAULT-SF-PURCD       PIC X(3).
           03  BR-DEFAULT-LN-UNITPER-CD  PIC X.
           03  BR-DEFAULT-LN-PURCD       PIC X(3).
           03  BR-TAP-OTHER-DL           PIC X.
           03  BR-AH-AND-CL              PIC X.
           03  BR-NO-CASH-PROCEEDS       PIC X.
               88  BR-NO-CASH-PROCEEDS-VALID  VALUE "Y" "N" "B".
               88  BR-CASH-PB-ONLY       VALUE "B".
           03  BR-RI-BEFORE-REPO         PIC X.
           03  BR-RI-BEFORE-P4P5         PIC X.
           03  BR-NOT-OURS-REFUND        PIC X.
           03  BR-SFDLO3-WINDOW-FG       PIC X.
           03  BR-CALC-INS-ADJ           PIC X.
           03  BR-AC-BEFORE-REPO         PIC X.
           03  BR-AC-BEFORE-P4P5         PIC X.
           03  BR-BANK-ROUTING-NO        PIC 9(9)   COMP-3.
           03  BR-BANK-CLEARING-GL       PIC 9(11)  COMP-3.
           03  BR-GLBANK-DEPOSIT         PIC 9(11)  COMP-3.
           03  BR-BANK-ACCTNO            PIC X(17).
           03  BR-DEFAULT-SC-SUBCLASS    PIC 9(3).
           03  BR-NO-OUT-DLR-RSV         PIC X.
           03  BR-COLLECTOR-QUEUE        PIC X.
           03  BR-COLL-QUEUE-CON-REC     PIC X.
           03  BR-JE-ALLOCATION-SKIP     PIC X.
           03  BR-EXPAND-APP-INPUT       PIC X.
           03  BR-A50-CPCORP             PIC X.
           03  BR-INBR-BILNSC-FG         PIC X.
               88  BR-INBR-BILNSC-FG-VALID   VALUE "Y" "N" "M".
      *        ---------------------------------------------------------
      *         "Y" : USE GLOBAL SCAN WINDOW FOR INTER BRANCH SCAN.
      *         "N" : DO NOT USE GLOBAL SCAN WITH INTERBRANCH POSTING
      *               POP WINDOE TO ENTER BRNO AND GO TO THAT BRANCH.
      *         "M" : SAME AS "N" ABOVE PLUS THE FOLLOWING: WHEN IN
      *               COLLECTIONS OR F10 INT. GLOBAL SCAN WILL BE
      *               REPLACED WITH AN ENTRY BOX FOR BRNO ACCTNO TO VIEW.
      *        ---------------------------------------------------------
           03  BR-INBR-BRIEF-FG          PIC X.
           03  BR-LEDGER-JUST-INTCHG     PIC X.
           03  BR-BYINQ-START-PAGE       PIC 99.
           03  BR-DSINQ-START-PAGE       PIC 99.
           03  BR-APP-REQUIRED-FOR-INPUT PIC X.
           03  BR-CREDIT-SCORING-FG      PIC X.
           03  BR-COL-QUEUE-LOW-TO-HI    PIC X.
           03  BR-CAN-GROUP-ID           PIC X(4).
           03  BR-AUTO-LNSTAT-WINDOW     PIC X.
           03  BR-TIME-ZONE-FACTOR       PIC S9.
           03  BR-LONPF-OPT-DISPLAY      PIC X.
           03  BR-GIFTNET                PIC X.
           03  BR-GIFTNET-LEDGER-LINE    PIC X.
           03  BR-NO-FT-EMP              PIC 9(3)  COMP-3.
           03  BR-NO-PT-EMP              PIC 99V99 COMP-3.
           03  BR-BALLOON-LOANS-FG       PIC X.
           03  BR-GL-TYPE-ALLOWED        PIC X.
           03  BR-GL-ENTRY-DEFAULT       PIC X.
           03  BR-MIDSTREAM-1STPAY-FG    PIC X.
           03  BR-NODEF-1ST-X-MONTHS     PIC 9.
           03  BR-ADD-INS-FEECODE        PIC XX.
           03  BR-ROAD-G-SGL-FEECODE     PIC XX.
           03  BR-ROAD-G-FAM-FEECODE     PIC XX.
           03  BR-NODEF-IF-ZERO-AMT      PIC X.
           03  BR-ASSESS-LATE-CHGS       PIC X.
           03  BR-NEXT-SOLICIT-NO        PIC 9(6) COMP-3.
           03  BR-ADDON-FEECD-O3         PIC XX OCCURS 2 TIMES.
           03  BR-IMMEDIATE-RECEIPT      PIC X.
           03  BR-IMMEDIATE-DOC-PRINT    PIC X.
           03  BR-AUTO-PL-REF-AC         PIC X.
               88  BR-AUTO-PL-REF-AC-VALID  VALUE "Y" "2" "3".
           03  BR-FAX-NUMBER             PIC 9(10)    COMP-3.
           03  BR-ALPHA-DRAWERS          PIC X.
           03  BR-LATECHG-ON-ALLOTMENTS  PIC X.
           03  BR-ALLOW-SYS-AS-EFFDATE   PIC X(01).
           03  BR-TAX-PREBOOK-CUST       PIC 9(4) COMP-3.
           03  BR-TAX-PREBOOK-NONCUST    PIC 9(4) COMP-3.
           03  BR-PASSWORD-DRAWER        PIC X.
           03  BR-ONESTEP-RN-FG          PIC X.
           03  BR-LI-LISCAN-FG           PIC X.
           03  BR-HIDE-END-BAL           PIC X(01).
      *     BATCH PAYMENT FIELDS (LOCK BOX PROCESSING):
           03  BR-LBOX-NOPAYOFF          PIC X.
           03  BR-LBOX-ODDPAY            PIC X.
           03  BR-LBOX-LC                PIC X.
           03  BR-LBOX-NOPRIN            PIC X.
           03  BR-LBOX-FLASH             PIC X.
           03  BR-LBOX-ACTIONCD          PIC X.
           03  BR-LBOX-MAXCON            PIC 9(3)     COMP-3.
           03  BR-LBOX-MAXREC            PIC 9(3)     COMP-3.
           03  BR-LBOX-PL                PIC X.
           03  BR-LBOX-JD                PIC X.
           03  BR-LBOX                   PIC X(01).
           03  BR-LATECHG-ON-LBOX        PIC X(01).
           03  BR-ALLOW-OV-TRCD          PIC X.
           03  BR-GLOBAL-QUEUE           PIC X(01).
           03  BR-LBOX-PAYOFF-NONCASH    PIC X.
           03  BR-LBOX-PAYOFF-OV         PIC X.

           03  BR-COLLECT-MANAGER      PIC X(25).
           03  BR-COLLECT-PHONE        PIC 9(10) COMP-3.
           03  BR-COLLECT-EXT          PIC 9(4)  COMP-3.

           03  BR-ALT-INTO-THIS-BRANCH PIC X.
           03  BR-POFF-TITLE-WINDOW    PIC X.
           03  BR-COUPON-STATEMENT     PIC X.
           03  BR-SUPERVISOR           PIC X(25).
           03  BR-AREA-MANAGER         PIC X(25).

           03  BR-MIN-MONTHS-DEF       PIC 99.

      * COLLECTION CENTER INFO, PR# 164 REGACC
           03  BR-CC-FLAG              PIC X.
           03  BR-CC-ADR1              PIC X(24).
           03  BR-CC-CITY              PIC X(15).
           03  BR-CC-STATE             PIC XX.
           03  BR-CC-ZIP               PIC X(10).
      *USED TO INTERRUPT GRADE CODES COMING FROM CMSI TO POPULATE 
      *PARTIC. HELD
           03  BR-MIN-MONTHS-D2          PIC 99. 
           03  BR-SKIP-MIDSTREAM-TRW     PIC X.
           03  BR-GL-ESCROW-BALANCE      PIC 9(11) COMP-3.
           03  BR-GL-ESCROW-PAYABLE      PIC 9(11) COMP-3.
           03  BR-ALLOW-FORCE-OPEN-FG    PIC X.
           03  BR-ACH-BANK-NO            PIC 9.
           03  BR-WIMENU-CHECK-WRITING   PIC X.
           03  BR-ADP-PAY-GROUP          PIC X(3).
           03  BR-ALLOW-SCANNED-CKS      PIC X.
           03  BR-CHECK-DEPOSIT-GL       PIC 9(11) COMP-3.
           03  BR-OPTION-PST-LASER       PIC X.
           03  BR-USE-DEBIT-CARDS        PIC X.
           03  BR-GL-DEBIT-CARD-PROCEEDS PIC 9(11) COMP-3.
           03  BR-WKDAY-BANKID           PIC 9(4) COMP-3.
           03  BR-PURGE-NOW-FLAG         PIC X.
           03  BR-EMAIL                  PIC X(50).
           03  BR-MANAGER-EMAIL          PIC X(50).
           03  BR-VERITEC-LICENSE        PIC 9(7).
           03  BR-TEST-PP-AUTO-INSCOVR   PIC X.
           03  BR-USE-NEW-WFARGO-ACCT    PIC X.
           03  BR-BRANCH-CLOSED          PIC X.
           03  BR-ALLOW-F3-ALL-DOCS      PIC X.

           03  FILLER                    PIC X(396).
