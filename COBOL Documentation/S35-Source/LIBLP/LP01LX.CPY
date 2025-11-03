      * COPYMEMBER: LIBLP/LP01LX
      ******************************************************************
      *
      * LOAN PAYMENT TRAILER FILE: 
      *
      *    LX1-KEY = LX-BRNO, LX-ACCTNO, LX-SEQNO
      *
      * CODE:
      *  A - ADDON TRAILER RECORD
      *  M - MONTHLY INSURANCE PREMIUM INFO
      *  Z - AMORTIZED INTEREST (Z) PAYMENT INFO
      *  T - TRANFER
      *  P - TRANFER (PURCHASED)
      *  X - INSURANCE TAX
      *  R - REBATE AMOUNT FOR GEORGIA 4% CLOSING FEE EARLY PROVISION
      *
      * REV:
      *  JTG 101196 ADDED 'LXE-WORKER' & 'LXE-WORKER2' FOR LPCERN-- A50 VERSION
      *                   SP-ERNFRMLA 15 & 16, REAL INTEREST BEARINGING METHOD
      *                                            - INSTANT AUTO, KY
      *  MJD 041797 ADDED LX-2 RECORD. EXTENSION OF LXA-PMR-SCHED-CHNG,
      *                   LEVELS 4-9
      *  JTG 112097 EXPANDED BY 10 BYTES
      *  KEC 990510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 990811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
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
      * BAH 190114 GLOBAL LXFILE, MOVED LX-BRNO UNDER LX1-KEY
      * BAH 190305 REMOVED FILLER FOR EACH CODE, ONLY HAVE FILLER AT END
      *  CS 190502 ADD FUTURE FIELDS
      *  CS 190502 TALKED WITH BETH, THE LX,LXE,LXG,LN,PD,LP ARE PASSED VIA
      *            LP01WK, A LOT OF EFFORT WAS PUT IN TO MAKE THE WORK RECORD
      *            ACCURATE, SO... KEEP THE FILE SIZES THE SAME SIZE AS THE
      *            WORK RECORD.
      * BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20210427 REMOVED FUTURE FIELDS
      * BAH 20230600 ADDED FILLER
      **************************************************************************
       78  LX-SIZE                 VALUE 530. 
      *-----------------------------------------------------------------
       01  LX-REC.
           03  LX1-KEY.
               05  LX-BRNO         PIC 9(4).
               05  LX-ACCTNO       PIC 9(8).
               05  LX-SEQNO        PIC 9(5).

      $XFD DATE=YYYYMMDD
           03  LX-LTOUCH-DATE      PIC 9(8) COMP-3.

      **************************************************
      *    T  - TRANSFER TRAILER RECORD.
      *  T   1 - 10  ANTIC          
      *  T  11 - 20 ANTIC-ADJ 
      **************************************************
           03  LXT-CODE            PIC X.
           03  LXT.
               05  LXT-EARN        PIC S9(7)V99  COMP-3 OCCURS 20.

      **************************************************
      *    P - TRANSFER TRAILER RECORD.
      *  P   1 - 10 ACCRERN
      *  P  11 - 20 UFERN(UPFRONT) 
      **************************************************
           03  LXP-CODE            PIC X.
           03  LXP.
               05  LXP-EARN        PIC S9(7)V99  COMP-3 OCCURS 20.

      **************************************************
      *    A - ADDON TRAILER RECORD.
      *                  (60 BYTES)
      *LXA-START-PMT HOLDS THE PAYMENT NUMBER WHERE THE NEW PAYMENT STARTS
      *LXA-END-PMT HOLD THE ENDING PAYMENT NUMBER FOR THE NEW PAYMENT.
      **************************************************
           03  LXA-CODE            PIC X.
           03  LXA.
               05  LXA-SRCD                PIC XX.

               05  LXA-PMT-SCHED-CHNG OCCURS 9.
                   07  LXA-START-PMT       PIC 99999     COMP-3.
                   07  LXA-END-PMT         PIC 99999     COMP-3.
                   07  LXA-NET-CHANGE      PIC S9(7)V99  COMP-3.

               05  LXA-LN-INSTBL.
                   07  LXA-LN-INSCOMP      PIC 99        COMP-3.
                   07  LXA-LN-INSOURS      PIC X.

      *-----------------------------------------------------------------
      * NOTE: THE USE OF LXA-LN-INSTRM CANNOT BE FOUND ANYWHERE IN
      *       A15 & A30.  I BELIEVE IT WAS ADDED AS TO MATCH UP
      *       WITH THE USE WITHIN THE LNFILE (LN-INSTRM).
      *       COMMENTING IT OUT TILL NEEDED.              JAY 2017-0508
      *-----------------------------------------------------------------
      $XFD DATE=YYYYMMDD
                   07  LXA-LN-INSEFF-DATE  PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  LXA-LN-INSEXP-DATE  PIC 9(8)      COMP-3.
                   07  LXA-LN-INSPREM      PIC S9(7)V99  COMP-3.
                   07  LXA-LN-INSCOVR      PIC S9(7)V99  COMP-3.
                   07  LXA-LN-INSCOMM      PIC S9(7)V99  COMP-3.

               05  LXA-LN-INS-REBATE       PIC X.
               05  LXA-LN-INSCODES         PIC X(3).

      **************************************************
      *    Z - AMORTIZED INTEREST (Z) PAYMENT INFO
      **************************************************
           03  LXZ-CODE         PIC X.
           03  LXZ.
               05  LXZ-POST-REGPY       PIC S9(7)V99 COMP-3.
               05  LXZ-POST-EXTRA-PRIN  PIC S9(7)V99 COMP-3.
               05  LXZ-POST-OTH         PIC S9(7)V99 COMP-3.
               05  LXZ-POST-LCHG        PIC S9(7)V99 COMP-3.

               05  LXZ-PDUE-APINT       PIC S9(7)V99 COMP-3.
               05  LXZ-PDUE-APPRIN      PIC S9(7)V99 COMP-3.
               05  LXZ-CUR-APINT        PIC S9(7)V99 COMP-3.
               05  LXZ-CUR-APPRIN       PIC S9(7)V99 COMP-3.


      **************************************************
      *    R - REBATE AMOUNT FOR 
      *        GEORGIA 4% CLOSING FEE EARLY PROVISION
      **************************************************
           03  LXR-CODE         PIC X.
           03  LXR.
               05  LXR-REB-AMT         PIC S9(7)V99  COMP-3.

      **************************************************
      *    X - INSURANCE TAX
      *                  (60 BYTES)
      **************************************************
           03  LXX-CODE         PIC X.
           03  LXX.
               05  LXX-INS-TAX-COUNTY-CD  PIC X(3).
               05  LXX-INS-TAX-CITY-CD    PIC X(3).


      **************************************************
      *    M - MONTHLY INSURANCE PREMIUM PAYMENT INFO:
      **************************************************
           03  LXM-CODE         PIC X.
           03  LXM.
               05  LXM-MIP-CLPAID      PIC S9(7)V99  COMP-3.
      $XFD DATE=YYYYMMDD
               05  LXM-MIP-CLPDTH-DATE PIC 9(8)      COMP-3.
               05  LXM-MIP-AHPAID      PIC S9(7)V99  COMP-3.
      $XFD DATE=YYYYMMDD
               05  LXM-MIP-AHPDTH-DATE PIC 9(8)      COMP-3.
           03  FILLER                 PIC X(97).
