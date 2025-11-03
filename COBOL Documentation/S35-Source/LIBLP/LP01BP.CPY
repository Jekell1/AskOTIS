      * COPYMEMBER: LIBLP/LP01BP
      *******************************************************
      *    BATCH PAYMENT RECORD
      *                  (220  BYTES)
      *
      *    FILE NAME:
      *              B      - BATCH PAYMENT FILE
      *              9999   - BRANCH NO
      *              MMDD   - DATE BATCH ESTABLISHED
      *              999    - BATCH NO
      *              REFCD  - BATCH REFCD       (OPTIONAL)
      *
      *         WHEN THE FILE IS CREATED FROM MAGNETIC
      *         TAPE THE BATCH NO AND DATE ARE SET TO ZERO
      *
      *    WHEN BP-LNNO = 0, IT IS A HEADER RECORD.
      *            TYPE = "M", IT WAS CREATED FROM MAG TAPE
      *                 = "I", IT WAS INPUT
      *
      *  MJD 000403 ADDED BP-SIGN (TFC PR#78)   1 : PRE ALLOTMENT DEBIT
      *                                         2 : ONE TIME ONLY DEBIT
      *                                         3 : RECURRING DEBIT
      *  MJD 000418 ADDED BP-SIGN = 4           4 : MELLON BANK LOCK BOX
      *             (MERC PR#227)
      *  JTG 010604 ADDED GP-TRUE-NEXT-BATCH           (MERC PR#243/251)
      *                   BP-SIGN = 5           5 : TRUE PAY
      *                   BP-SIGN = 6           6 : TRUE COLLECT
      *                   BP-SIGN = 7           7 : MAIL GRAM
      *  BLF 051107 ADDED BP-REFCD AND ADDED TO FILLER TO EXPAND RECORD SIZE
      *             FROM 24 TO 50 BYTES, REGACC PR# 107
      *  BLF 060314 ADDED BP-PAYDATE, LENDMARK PR# 2711, NOT SURE IF I WILL
      *             USE HERE, BUT SEEMS LIKE WE SHOULD HAVE ALWAYS HAD THIS
      *             SINCE MOST INPUT (BTFILES) CONTAIN THE REAL PAYMENT DATE;
      *             FOR NOW WILL CHECK IN LONPW9, IF 0 OR 202020 OR 020202
      *             WILL FORCE TRANS-DATE TO BP-PAYDATE.
      *  BLF 070411 ADDED BP-SALES-AMT, BP-AUCTION-FEES, BP-AUCTION-NAME,
      *             REGACC PR# 168 (POST SS IN BATCH)
      *   CS 070418 REMOVED BP-SALES-AMT, CAN CALCULATE. ADDED 50 BYTES TO
      *             HANDLE THE AUCTION NAME PR#168 REGACC
      *  BLM 080605 FOR WORLD CLAIMS, ADDED BP-CLAIM-NUMBER, BP-PMT-TYPE,
      *             WILL PRINT ON EXCEPTION REPORT, UPDATED BY SP/LOTSBP,
      *             PRINTED BY LONPW9, WORLD PR# 573
      *   CS 081024 ADDED 2 FLAGS FOR POSTING 'DF' BP-IGNORE-DEFPOL & BP-
      *             ALLOW-ZERO-DF PR#219 REGACC
      *  BLM 120423 ADD BP-CREATE-ACH-FG TO INDICATE RECORD WAS CREATED BY
      *             EPEXTR & WILL CREATE ACH RECORD IN ACHEXT TO TRANSMIT
      *             TO BANK.  NEED TO STOP CREATION OF "OV" TRANSACTION
      *             FOR THESE RECORDS, CITIZENS
      *  BLM 160426 ADD BP-BNKRPT-CASE-NO & BP-BNKRPT-STATUS, WORLD PR# 1151
      *  BLM 160622 ADD BP-SEQNO FOR DUPLICAT BATCH PMTS TO THE SAME LOAN,
      *             PD0003
      *  BLM 160728 ADD BP-ALLOW-NEGATIVE, REGACC PR# 382
      *  BLM 161024 FIX BP-HEAD-REC REDEF, FILLER NEEDED EXPANDING BECAUSE
      *             OF BP-SEQNO, PD0003
      * CS 20201008 ADD BP-FROZEN-OVERRIDE, WHEN SET TO "Y", WILL OVERRIDE
      *             LN-ACCT-FROZEN = "Y" PR#1465
      *  BLM 220323 ADD BP-PL-REASON, WORLD PR# 1548
      *  BAH 2023.1011 COMMENTED BP-CREATE-ACH-FG AS UNUSED
      **************************************************************************
       78  BP-SIZE                 VALUE 220.
       01  BP-REC.
           03  BP1-KEY.
               05  BP-LNNO         PIC 9(8).
               05  BP-SEQNO        PIC 99.
           03  BP-SSNO             PIC 9(9)     COMP-3.
           03  BP-TRCD             PIC XX.
           03  BP-TRAMT            PIC S9(7)V99 COMP-3.
           03  BP-UPDFG            PIC X.
           03  BP-DUAL-LN          PIC X.
      *    -------------------------------------------------------------
      *    BP-SIGN IS A TFC OPTION  0 : (DEFAULT) STD. ALLOTMENT
      *                             1 : PRE ALLOTMENT DEBIT (PREEZ)
      *                             2 : ONE TIME ONLY DEBIT (OTOEZ)
      *                             3 : RECURRING DEBIT     (RECEZ)
      *                             4 : MELLON BANK LOCK BOX PAYMENTS
      *                             5 : TRUE PAY
      *                             6 : TRUE COLLECT
      *                             7 : MAIL GRAM
      *    -------------------------------------------------------------
           03  BP-SIGN             PIC 9.
           03  BP-REFCD            PIC X(5).
           03  BP-PAYDATE          PIC 9(8)     COMP-3.
           03  BP-AUCTION-FEES     PIC S9(7)V99 COMP-3.
           03  BP-AUCTION-NAME     PIC X(30).
           03  BP-WORLD-CLAIM-FIELDS.
                05  BP-CLAIM-NUMBER PIC 9(8)    COMP-3.
                05  BP-PMT-TYPE    PIC X(10).
           03  BP-IGNORE-DEFPOL    PIC X.
           03  BP-ALLOW-ZERO-DF    PIC X.
      * BP-CREATE-ACH-FG NOT USED ANYMORE 2023.1011
           03  BP-CREATE-ACH-FG    PIC X.
           03  BP-BNKRPT-CASE-NO   PIC X(9).
           03  BP-BNKRPT-STATUS    PIC X.
           03  BP-ALLOW-NEGATIVE   PIC X.
           03  BP-FROZEN-OVERRIDE  PIC X.
           03  BP-PL-REASON        PIC XX.
           03  FILLER              PIC X(118).
      *--------------------------------------------------------
       01  BP-HEAD-REC.
           03  FILLER              PIC X(10).
           03  BP-TYPE             PIC X.
           03  BP-TOTAL-COUNT      PIC S9(7)    COMP-3.
           03  BP-TOTAL-AMOUNT     PIC S9(9)V99 COMP-3.
           03  FILLER              PIC X(199).
