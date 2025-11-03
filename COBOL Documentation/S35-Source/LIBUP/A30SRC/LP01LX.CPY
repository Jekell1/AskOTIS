      * COPYMEMBER: LIBLP/LP01LX
      ******************************************************************
      *
      * LOAN PAYMENT ADDON FILE: (300 BYTES)
      * KEY 1 -  LOAN #, SEQUENCE #, CODE
      *
      * CODE:
      *  A - ADDON TRAILER RECORD
      *  2 - ADDON TRAILER RECORD 2
      *  G - GENERAL LEDGER DISTRIBUTION RECORD
      *  E - EARNINGS TRAILER RECORD
      *  M - MONTHLY INSURANCE PREMIUM INFO
      *  Z - AMORTIZED INTEREST (Z) PAYMENT INFO
      *  N - NON SPECIFIC GENERAL INFORMATION RECORD  *NOT USED*
      *  T - TRANFER
      *  X - INSURANCE TAX
      *  R - REBATE AMOUNT FOR GEORGIA 4% CLOSING FEE EARLY PROVISION
      *
      * NOTE: LX-CODE OF "2" IS NO LONGER USED.  IT WAS MERGED INTO
      *       LX-CODE OF "A" IN THE A15-TO-A30 CONVERSION (UP/UL1530).
      *
      * NOTE: LX-CODE OF "N" IS NOT USED.  SEARCHED IN A10, A60 AND A48
      *       AND CANNOT FIND WHO ADDED (WAS IN A60 AS OF 1996-1015),
      *       NO DOCUMENTATION OF ITS USE, AND ITS USE
      *       (MOVE "N" TO LX-CODE) ANYWHERE.  SO COMMENTING IT OUT
      *       UNTIL PROVEN USAGE CAN BE FOUND.  JAY 2017-0508.
      *
      *=================================================================
      * REV:
      *  JTG 101196 ADDED 'LXE-WORKER' & 'LXE-WORKER2' FOR LPCERN-- A50 VERSION
      *                   SP-ERNFRMLA 15 & 16, REAL INTEREST BEARINGING METHOD
      *                                            - INSTANT AUTO, KY
      *  MJD 041797 ADDED LX-2 RECORD. EXTENSION OF LXA-PMR-SCHED-CHNG,
      *                   LEVELS 4-9
      *  JTG 112097 EXPANDED BY 10 BYTES
      *  KEC 990510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 990811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 001206 ADDED LX-SIZE                           RSZ=0073
      *  JTG 011004 ADDED LX-Z RECORD AMORTIZED INTEREST (Z) PAYMENT INFO
      *  JTG 020603 ADDED LX-M RECORD MONTHLY INSURANCE PREMIUM PAYMENT INFO
      *                                                        REGENCY #1801
      *  CS  021120 ADDED LX-X RECORD INSURANCE TAX [WORLD PR#288]
      *  CS  151020 CHANGED LX-SEQ TO 9(5)
      *  CS  160209 ADD LX-BRNO,CHANGED LXA-START-PMT/LXA-END-PMT TO 9(5), 
      *             LX2-START-PMT,LX2-END-PMT TO 9(5). THIS HOLDS THE PAYMENT
      *             NUMBER WHERE THE NEW PAYMENT STARTS & STOPS.
      *             ADDED LP-EARN BUCKETS 7-10  (NEW EARNINGS)
      *  CS  160210 COMBINED LXA AND LX2
      *  MJD 160613 ADDED LX-R RECORD. THIS IS THE AMOUNT
      *             OF THE SERVICE CHARGE THAT
      *             IS THE GEORGIA 4% CLOSING FEE EARLY PROVISION CALCULATED 
      *             IN REBATE-EP54 (LIBGB/REBATE).
      *             THIS IS INCLUDED IN THE SERVICE CHARGE 
      *             REBATE BUT NEEDS TO BE BROKEN OUT ON CERTAIN CONTRACTS. 
      *             IT WILL BE STORED HERE IF NOT = 0               REGMGT #4915
      **************************************************************************
       78  LX-SIZE                                           VALUE 300. 
      *-----------------------------------------------------------------
       01  LX-REC.
           03  LX-BRNO            PIC 9(4).
           03  LX1-KEY.
               05  LX-ACCTNO       PIC 9(8).
               05  LX-SEQNO        PIC 9(5).
               05  LX-CODE         PIC X.
      *>>XDD DATE=YYYYMMDD
           03  LX-LTOUCH-DATE      PIC 9(8) COMP-3.
      **************************************************
      *    G - GENERAL LEDGER DISTRIBUTION RECORD:
      **************************************************
           03  LX-G.
               05  LXG-GLTBL           OCCURS 3 TIMES.
                   07  LXG-GLNO        PIC 9(11)     COMP-3.
                   07  LXG-GLAMT       PIC S9(7)V99  COMP-3.
               05  FILLER              PIC X(244).

      **************************************************
      *    E - EARNINGS TRAILER RECORD.
      *                  
      * EARNINGS 4 - 10
      **************************************************
           03  LX-E                   REDEFINES LX-G.
               05  LXE-EARN            PIC S9(7)V99  COMP-3
                                       OCCURS 7 TIMES.
      *-------------------------------------------------------
      *       WHEN SP-ERNFRMLA = 15 & 16, THIS FIELD HOLDS IB-EARNINGS
      *       INTEREST OWING BEFORE THIS TRANSACTION
               05  LXE-WORKER          PIC S9(7)V99  COMP-3.
      *       WHEN SP-ERNFRMLA = 15 & 16, THIS FIELD HOLDS IB-EARNINGS
      *       INTEREST TAKEN ON THIS TRANSACTION
               05  LXE-WORKER2         PIC S9(7)V99  COMP-3.
      *-------------------------------------------------------
               05  FILLER              PIC X(232). 

      **************************************************
      *    T & P - TRANSFER TRAILER RECORD.
      *                  (60 BYTES)
      *  T   1 - 10  ANTIC          P   1 - 10 ACCRERN
      *  T  11 - 20 ANTIC-ADJ       P  11 - 20 UFERN(UPFRONT) 
      **************************************************
           03  LX-T                   REDEFINES LX-G.
               05  LXT-EARN            PIC S9(7)V99  COMP-3 
                                       OCCURS 20 TIMES.
               05  FILLER              PIC X(177).

      **************************************************
      *    A - ADDON TRAILER RECORD.
      *                  (60 BYTES)
      *LXA-START-PMT HOLDS THE PAYMENT NUMBER WHERE THE NEW PAYMENT STARTS
      *LXA-END-PMT HOLD THE ENDING PAYMENT NUMBER FOR THE NEW PAYMENT.
      **************************************************
           03  LX-A                   REDEFINES LX-G.
               05  LXA-SRCD            PIC XX.
               05  LXA-PMT-SCHED-CHNG  OCCURS 9 TIMES.
                   07  LXA-START-PMT   PIC 99999     COMP-3.
                   07  LXA-END-PMT     PIC 99999     COMP-3.
                   07  LXA-NET-CHANGE  PIC S9(7)V99  COMP-3.
               05  LXA-LN-INSTBL.
                   07  LXA-LN-INSCOMP  PIC 99        COMP-3.
                   07  LXA-LN-INSOURS  PIC X.

      *-----------------------------------------------------------------
      * NOTE: THE USE OF LXA-LN-INSTRM CANNOT BE FOUND ANYWHERE IN
      *       A15 & A30.  I BELIEVE IT WAS ADDED AS TO MATCH UP
      *       WITH THE USE WITHIN THE LNFILE (LN-INSTRM).
      *       COMMENTING IT OUT TILL NEEDED.              JAY 2017-0508
      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
                   07  LXA-LN-INSEFF-DATE   PIC 9(8)      COMP-3.
      *KC>         07  LXA-LN-INSTRM  REDEFINES LXA-LN-INSEFF-DATE
      *KC>                                  PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
                   07  LXA-LN-INSEXP-DATE   PIC 9(8)      COMP-3.
                   07  LXA-LN-INSPREM  PIC S9(7)V99  COMP-3.
                   07  LXA-LN-INSCOVR  PIC S9(7)V99  COMP-3.
                   07  LXA-LN-INSCOMM  PIC S9(7)V99  COMP-3.
               05  LXA-LN-INS-REBATE   PIC X.
               05  LXA-LN-INSCODES     PIC X(3).
               05  FILLER              PIC X(144).


      **************************************************
      *    Z - AMORTIZED INTEREST (Z) PAYMENT INFO
      *                  (60 BYTES)
      **************************************************
           03  LX-Z                    REDEFINES LX-G.
               05  LXZ-POST-REGPY       PIC S9(7)V99 COMP-3.
               05  LXZ-POST-EXTRA-PRIN  PIC S9(7)V99 COMP-3.
               05  LXZ-POST-OTH         PIC S9(7)V99 COMP-3.
               05  LXZ-POST-LCHG        PIC S9(7)V99 COMP-3.

               05  LXZ-PDUE-APINT       PIC S9(7)V99 COMP-3.
               05  LXZ-PDUE-APPRIN      PIC S9(7)V99 COMP-3.
               05  LXZ-CUR-APINT        PIC S9(7)V99 COMP-3.
               05  LXZ-CUR-APPRIN       PIC S9(7)V99 COMP-3.

               05  FILLER               PIC X(237).

1801  **************************************************
1801  *    M - MONTHLY INSURANCE PREMIUM PAYMENT INFO:
1801  *                  (60 BYTES)
1801  **************************************************
1801       03  LX-M                   REDEFINES LX-G.
1801           05  LXM-MIP-CLPAID      PIC S9(7)V99  COMP-3.

      *>>XDD DATE=YYYYMMDD
1801           05  LXM-MIP-CLPDTH-DATE      PIC 9(8)      COMP-3.
1801           05  LXM-MIP-AHPAID      PIC S9(7)V99  COMP-3.

      *>>XDD DATE=YYYYMMDD
1801           05  LXM-MIP-AHPDTH-DATE      PIC 9(8)      COMP-3.
1801
1801           05  FILLER              PIC X(257).

      ******************************************************************
      *    N - NON SPECIFIC GENERAL INFORMATION RECORD
      *-----------------------------------------------------------------
      * NOTE: LX-CODE OF "N" IS NOT USED.  SEARCHED IN A10, A60 AND A48
      *       AND CANNOT FIND WHO ADDED (WAS IN A60 AS OF 1996-1015),
      *       NO DOCUMENTATION OF ITS USE, AND ITS USE
      *       (MOVE "N" TO LX-CODE) ANYWHERE.  SO COMMENTING IT OUT
      *       UNTIL PROVEN USAGE CAN BE FOUND.  JAY 2017-0508.
      *
      ******************************************************************
      *    03  LX-N                   REDEFINES LX-G.
      *        05  FILLER              PIC X(277).
               
      **************************************************
      *    R - REBATE AMOUNT FOR 
      *        GEORGIA 4% CLOSING FEE EARLY PROVISION
      **************************************************
           03  LX-R                   REDEFINES LX-G.
               05  LXR-REB-AMT         PIC S9(7)V99  COMP-3.
               05  FILLER              PIC X(272).

      **************************************************
      *    X - INSURANCE TAX
      *                  (60 BYTES)
      **************************************************
           03  LX-X                   REDEFINES LX-G.
               05  LXX-INS-TAX-COUNTY-CD  PIC X(3).
               05  LXX-INS-TAX-CITY-CD    PIC X(3).

               05  FILLER                 PIC X(271).
