      * COPYMEMBER: LIBLP/LP01LX
      ******************************************************************
      *    LOAN PAYMENT ADDON FILE: (73 BYTES)
      *    KEY 1 -  LOAN #, SEQUENCE #, CODE
      *
      *    CODE:
      *         A - ADDON TRAILER RECORD
      *         2 - ADDON TRAILER RECORD 2
      *         G - GENERAL LEDGER DISTRIBUTION RECORD
      *         E - EARNINGS TRAILER RECORD
      *         M - MONTHLY INSURANCE PREMIUM INFO
      *         Z - AMORTIZED INTEREST (Z) PAYMENT INFO
      *         N - NON SPECIFIC GENERAL INFORMATION RECORD
      *         T - TRANFER                   
      *         X - INSURANCE TAX
      *         R - Rebate amount for GEORGIA 4% CLOSING FEE EARLY PROVISION
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
      *  mjd 160209 added LX-R record. This is the amount of the service charge that
      *             is the GEORGIA 4% CLOSING FEE EARLY PROVISION calculated 
      *             in REBATE-EP54 (LIBGB/REBATE).  This is included in the service charge 
      *             rebate but needs to be broken out on certain contracts. 
      *             It will be stored here if not = 0               RegMgt #4915
      **************************************************************************
       78  LX-SIZE                                             VALUE 73.
      *-----------------------------------------------------------------
       01  LX-REC.
           03  LX1-KEY.
               05  LX-ACCTNO       PIC 9(8).
               05  LX-SEQNO        PIC 9(4).
               05  LX-CODE         PIC X.
      **************************************************
      *    G - GENERAL LEDGER DISTRIBUTION RECORD:
      *                  (60 BYTES)
      **************************************************
           03  LX-G.
               05  LXG-GLTBL           OCCURS 3 TIMES.
                   07  LXG-GLNO        PIC 9(11)     COMP-3.
                   07  LXG-GLAMT       PIC S9(5)V99  COMP-3.
               05  FILLER              PIC X(30).

      **************************************************
      *    E - EARNINGS TRAILER RECORD.
      *                  (60 BYTES)
      **************************************************
           03  LX-E                   REDEFINES LX-G.
               05  LXE-EARN            PIC S9(5)V99  COMP-3
                                       OCCURS 3 TIMES.
      *-------------------------------------------------------
      *       WHEN SP-ERNFRMLA = 15 & 16, THIS FIELD HOLDS IB-EARNINGS
      *       INTEREST OWING BEFORE THIS TRANSACTION 
               05  LXE-WORKER          PIC S9(7)V99  COMP.
      *       WHEN SP-ERNFRMLA = 15 & 16, THIS FIELD HOLDS IB-EARNINGS
      *       INTEREST TAKEN ON THIS TRANSACTION 
               05  LXE-WORKER2         PIC S9(7)V99  COMP.
      *-------------------------------------------------------
               05  FILLER              PIC X(40).

      **************************************************
      *    T & P - TRANSFER TRAILER RECORD.
      *                  (60 BYTES)
      **************************************************
           03  LX-T                   REDEFINES LX-G.
               05  LXT-EARN            PIC S9(7)V99  COMP
                                       OCCURS 12 TIMES.
               05  FILLER              PIC X(12).

      **************************************************
      *    A - ADDON TRAILER RECORD.
      *                  (60 BYTES)
      **************************************************
           03  LX-A                   REDEFINES LX-G.
               05  LXA-SRCD            PIC XX.
               05  LXA-PMT-SCHED-CHNG  OCCURS 3 TIMES.
                   07  LXA-START-PMT   PIC 999       COMP-3.
                   07  LXA-END-PMT     PIC 999       COMP-3.
                   07  LXA-NET-CHANGE  PIC S9(6)V99  COMP.
               05  LXA-LN-INSTBL.
                   07  LXA-LN-INSCOMP  PIC 99        COMP.
                   07  LXA-LN-INSOURS  PIC X.
                   07  LXA-LN-INSEFF   PIC 9(6)      COMP.   
                   07  LXA-LN-INSTRM  REDEFINES LXA-LN-INSEFF
                                       PIC 9(6)      COMP.
                   07  LXA-LN-INSEXP   PIC 9(6)      COMP.   
                   07  LXA-LN-INSPREM  PIC S9(4)V99  COMP.
                   07  LXA-LN-INSCOVR  PIC S9(6)V99  COMP.
                   07  LXA-LN-INSCOMM  PIC S9(4)V99  COMP.
               05  LXA-LN-INS-REBATE   PIC X.
               05  LXA-LN-INSCODES     PIC X(3).
               05  FILLER              PIC X(12).
      **************************************************
      *    2 - ADDON TRAILER RECORD 2.
      *                  (60 BYTES)
      **************************************************
           03  LX-2                   REDEFINES LX-G.
               05  LX2-PMT-SCHED-CHNG  OCCURS 6 TIMES.
                   07  LX2-START-PMT   PIC 999       COMP-3.
                   07  LX2-END-PMT     PIC 999       COMP-3.
                   07  LX2-NET-CHANGE  PIC S9(6)V99  COMP.
               05  FILLER              PIC X(12).
      **************************************************
      *    Z - AMORTIZED INTEREST (Z) PAYMENT INFO
      *                  (60 BYTES)
      **************************************************
           03  LX-Z                    REDEFINES LX-G.
               05  LXZ-POST-REGPY       PIC S9(7)V99 COMP.
               05  LXZ-POST-EXTRA-PRIN  PIC S9(7)V99 COMP.
               05  LXZ-POST-OTH         PIC S9(7)V99 COMP.
               05  LXZ-POST-LCHG        PIC S9(7)V99 COMP.

               05  LXZ-PDUE-APINT       PIC S9(7)V99 COMP.
               05  LXZ-PDUE-APPRIN      PIC S9(7)V99 COMP.
               05  LXZ-CUR-APINT        PIC S9(7)V99 COMP.
               05  LXZ-CUR-APPRIN       PIC S9(7)V99 COMP.

               05  FILLER               PIC X(28).

1801  **************************************************
1801  *    M - MONTHLY INSURANCE PREMIUM PAYMENT INFO:
1801  *                  (60 BYTES)
1801  **************************************************
1801       03  LX-M                   REDEFINES LX-G.
1801           05  LXM-MIP-CLPAID      PIC S9(7)V99  COMP-3.
1801           05  LXM-MIP-CLPDTH      PIC 9(6)      COMP-6.
1801           05  LXM-MIP-AHPAID      PIC S9(7)V99  COMP-3.
1801           05  LXM-MIP-AHPDTH      PIC 9(6)      COMP-6.
1801
1801           05  FILLER              PIC X(6).

      **************************************************
      *    N - NON SPECIFIC GENERAL INFORMATION RECORD
      *                  (60 BYTES)
      **************************************************
           03  LX-N                   REDEFINES LX-G.
               05  FILLER              PIC X(60).
               
      **************************************************
      *    R - Rebate amount for 
      *        GEORGIA 4% CLOSING FEE EARLY PROVISION
      *                  (60 BYTES)
      **************************************************
           03  LX-R                   REDEFINES LX-G.
               05  LXR-REB-AMT         PIC S9(7)V99  COMP-3.
               05  FILLER              PIC X(55).

      **************************************************
      *    X - INSURANCE TAX  
      *                  (60 BYTES)
      **************************************************
           03  LX-X                   REDEFINES LX-G.
               05  LXX-INS-TAX-COUNTY-CD  PIC X(3).
               05  LXX-INS-TAX-CITY-CD    PIC X(3).
               05  FILLER                 PIC X(54).
