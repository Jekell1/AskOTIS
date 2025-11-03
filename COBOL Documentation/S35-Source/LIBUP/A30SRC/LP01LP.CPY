      * COPYMEMBER: LIBLP/LP01LP
      **************************************************************************
      *    LOAN PAYMENT FILE:   (410 BYTES)
      *
      *         KEY 1 - LOAN #, SEQUENCE #  (ASCENDING)
      *
      *  REV:
      *  KEC 052998 CHANGED ALL AMOUNT FIELDS TO BE '9(7)' PER JOHN.
      *              THE RECORD SIZE WAS NOT CHANGED; FILLER AT THE
      *              BOTTOM WAS USED.  FIX: FX/BY1010.C
      *  MJD 060498 EXPANDED RECORD SIZE TO 239 BYTES, ADDED LP-SIZE.
      *  BAH 062498 CHANGED LP-TILLNO TO ALPHANUMERIC
      *  MJD 062598 REDEFINE FIELDS UNDER LCWORK1 AND LCWORK2 DID
      *             NOT MATCH ORIG FIELDI SIZE.
      *  CS  081998 ADDED LP-POSTING-BR
      *  JTG 082698 CHANGED LP-PRINBAL TO LP-CURBAL, REGENCY REVOLVING
      *  JTG 093098 ALLOWED LN-CURBAL & LN-PURBAL TO GO NEGATIVE; REGENCY REVOL
      *  JTG 110698 CHANGED LP-APPRIN TO LP-APCUR, REGENCY REVOLVING
      *  JTG 111298 ADDED LP-RT-SEQNO  REVOLVING REGENCY
      *  JTG 120798 ADDED LP-REVOL-OTHCD REVOLVING OTHER (OT) CHARGES
      *  MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  CS  1999.0315 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  JTG 990729 REMOVED LP-REVOL-OTHCD, ADDED CD-RO-NUM TO PROVIDE
      *            AN ALPHA KEY RE: CD-RO-CODE
      *  KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  MJD 000119 ADDED LP-NOLCHG (WORLD PR#218)
      *  MJD 001016 ADDED LP-COLLECTOR (BAY PR#1317)
      *  JTG 010227 ADDED LN-EXTRA-PRIN-LFP LN-REMAIN-INT-SINCE-LFP
      *             LN-REMAIN-PRIN-SINCE-LFP  (LFP=LAST-FULL-PAYMENT)
      *             LENDMARK, PR#1045 & PR#1380
      * JTG 020308 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *            OPENED UP USE OF LP-PRORATED-INS-REB TO HOLD PRORATED PORTION
      *            OF INSURANCE REFUND                                WORLD #288
      * BLM 100305 ADDED LP-APPLIED-TO-ESCROW, REDEFINED AS LP-ESCROW-PREPAID,
      *            REGWAN PR# 3506
      **************************************************************************
      * A30 CHANGES   10/15/15
      * BLM 151015 ADDED UNUSED, INCREASED DATES TO 9(8), CHANGED COMP-6 TO
      *            COMP-3 FIELDS, INCREASED FIELD SIZES, CHANGED SEVERAL
      *            REDEFS TO BE SEPARATE FIELDS, ADDED LP-INT-PDTH,
      *            SUPPORT PR# PR0003
      *  CS 151016 ADD LP-ADDON-LAST-PYAMT WAS REDEFINE OF LC-WORK2.
      *  CS 151029 CHANGED LP-SIZE FROM 266 TO 371
      *  CS 151116 ADD BACK LP-ADDON-INTPDTH, SINCE ITS THE PRIOR INTEREST
      *            PAID THRU
      *  CS 151204 REMOVED LP-BRNO AS PART OF KEY
      *  CS 160201 REMOVED LP-T, REMOVED LP-FROMCL & LP-TOCL FROM REDEFINE AND
      *            MADE SEPARATE FIELDS, BOTH ARE NOW 9(3)
      * BAH 160304 CHANGED LP-TILLNO TO PIC 9(2) FROM PIC X.
      *  CS 160302 REDEFINE LP-USERID FOR LP-ALT-PREPAY-FG WHICH IS SET TO "Y"
      *            WHEN PDTH-PAYS > SP-ALT-PREPAY-MONTHS  WORLD PR#1140
      *  CS 160414 ADDED LP-ALT-PREPAY-FG AS ITS OWN FIELD
      **************************************************************************
       78  LP-SIZE                     VALUE 410.
       01  LP-REC.
           03  LP-BRNO                 PIC  9(4).
           03  LP1-KEY.
               05  LP-ACCTNO           PIC  9(8).
               05  LP-SEQNO            PIC  9(5).
           03  LP1-KEY9             REDEFINES LP1-KEY
                                       PIC  9(13).
      *-----------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  LP-LTOUCH-DATE          PIC 9(8)      COMP-3.
           03  LP-USERID               PIC X(10).
      *>>XDD DATE=YYYYMMDD
           03  LP-POSTDATE             PIC 9(8)      COMP-3.
           03  LP-POSTTIME             PIC 9(6)      COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LP-TRDATE               PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LP-PAYDATE              PIC 9(8)      COMP-3.
           03  LP-TRCD                 PIC XX.
           03  LP-REFNO                PIC X(5).
           03  LP-POOLID            REDEFINES LP-REFNO
                                       PIC  9(5).
           03  LP-DDNO              REDEFINES LP-REFNO.
               05  LP-FROMDD           PIC  99.
               05  LP-T                PIC X.
               05  LP-TODD             PIC  99.
           03  LP-ADDON               REDEFINES LP-REFNO.
               05  LP-ADDON-TEXT       PIC X(3).
               05  LP-ADDON-TYPE       PIC X(2).
           03  FILLER                 REDEFINES LP-REFNO.
               05  LP-DLRVNO           PIC 9.
               05  FILLER              PIC X(4).
           03  FILLER                 REDEFINES LP-REFNO.
               05  LP-P4P5-TRCD        PIC XX.
               05  LP-P4P5-DLRVNO      PIC 9.
               05  FILLER              PIC X(2).
      *-----------------------------------------------------
           03  LP-TRAMT                PIC S9(7)V99  COMP-3.
           03  LP-APOTH                PIC S9(7)V99  COMP-3.
           03  LP-APOT2                PIC S9(7)V99  COMP-3.
           03  LP-APINT                PIC S9(7)V99  COMP-3.
           03  LP-APLC                 PIC S9(7)V99  COMP-3.
           03  LP-APCUR                PIC S9(7)V99  COMP-3.
      *-----------------------------------------------------
           03  LP-OTHBAL               PIC S9(7)V99  COMP-3.
           03  LP-OT2BAL               PIC S9(7)V99  COMP-3.
           03  LP-INTBAL               PIC S9(7)V99  COMP-3.
           03  LP-LCBAL                PIC S9(7)V99  COMP-3.
           03  LP-CURBAL               PIC S9(7)V99  COMP-3.
      *-----------------------------------------------------
           03  LP-INTDUE               PIC S9(7)V99  COMP-3.
           03  LP-DLPROC               PIC S9(7)V99  COMP-3.
           03  LP-EARNED               OCCURS 3
                                       PIC S9(7)V99  COMP-3.
           03  LP-INTPAID              PIC S9(7)V99  COMP-3.
           03  LP-SSNO                 PIC  9(10)    COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LP-PDTH-DATE            PIC  9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LP-LCPDTH-DATE          PIC  9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LP-INTPDTH-DATE         PIC  9(8)     COMP-3.
           03  LP-TILLNO               PIC  99.
           03  LP-APINTOWE             PIC S9(7)V99  COMP-3.

           03  LP-DLPARVHELD-ADJ       PIC S9(7)V99  COMP-3.
           03  LP-LT-SEQNO             PIC 9(3)      COMP-3.
           03  LP-IBPC                 PIC X.
           03  LP-MANUAL-RB            PIC X.
           03  LP-INSTYPES             PIC X.
           03  LP-REV                  PIC X.
           03  LP-REVTRCD              PIC XX.
           03  LP-REVSEQNO             PIC 9(5)      COMP-3.
           03  LP-STATUSFG             PIC X.
           03  LP-NEWSTAT-RATE         PIC 999V999   COMP-3.
           03  LP-LCPARTIALS           PIC S9(7)V99  COMP-3.
           03  LP-ADDON-ORIG-REGPY     PIC S9(7)V99  COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LP-NEW1ST-PYDATE        PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LP-DEF-DUEDATE          PIC 9(8)      COMP-3.
      * LP-LCPAID CAN BE A DATE IN TRIPLE UCC STATES, MMDDYY
      * COULD NOT IDENTIFY HOW TO SPLIT THIS OUT VIA CONVERSION SO LEAVING
      * IT LIKE THIS, MUST STAY AS MMDDYY, USED AS DATE IN DEFI01.CBL
           03  LP-LCPAID               PIC S9(7)V99  COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LP-OLD1ST-PYDATE        PIC 9(8)      COMP-3.
           03  LP-DEF-TERM             PIC 9(3)      COMP-3.
           03  LP-ADDON-LAST-PYAMT     PIC S9(7)V99  COMP-3.
           03  LP-DLRVCHGS-ADJ         OCCURS 3
                                       PIC S9(7)V99  COMP-3.
           03  LP-DLPARVPAID-ADJ       PIC S9(7)V99  COMP-3.
           03  LP-RT-SEQNO             PIC 9(5)      COMP-3.
      * LP-DLNO MUST BE '0' UNLESS A PAYMENT FROM RESERVE
      * WAS POSTED FROM A DEALER OTHER THAN THE DEALER WHO
      * THE CONTRACT WAS PURCHASED FROM.
           03  LP-DLNO                  PIC 9(4)V99   COMP-3.
           03  LP-PREPAY-PENALTY        PIC S9(7)V99  COMP-3.
           03  LP-PRORATED-INS-REB      PIC S9(7)V99  COMP-3.
           03  LP-POSTING-BR            PIC 9(4)      COMP-3.
           03  LP-NOLCHG                PIC S9(3)     COMP-3.
           03  LP-COLLECTOR             PIC X(3).
           03  LP-EXTRA-PRIN-LFP        PIC S9(7)V99  COMP-3.
           03  LP-REMAIN-INT-SINCE-LFP  PIC S9(7)V99  COMP-3.
           03  LP-REMAIN-PRIN-SINCE-LFP PIC S9(7)V99  COMP-3.
           03  LP-APPLIED-TO-ESCROW     PIC S9(7)V99  COMP-3.
           03  LP-ESCROW-PREPAID        PIC S9(7)V99  COMP-3.
           03  LP-PRIOR-POOLID          PIC 9(5)      COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LP-ADDON-INTPDTH-DATE    PIC 9(8)      COMP-3.
           03  LP-FROMCL                PIC  999      COMP-3.
           03  LP-TOCL                  PIC  999      COMP-3.
           03  LP-ALT-PREPAY-FG         PIC X.
           03  FILLER                   PIC X(98).
