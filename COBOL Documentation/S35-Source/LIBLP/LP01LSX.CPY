      * COPYMEMBER: LIBLP/LP01LSX
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : (1800) BYTES
      *
      *       REPOSSESION INFO RECORD
      *
      *    LSX1-KEY = LSX-BRNO, LSX-ACCTNO, LSX-SEQNO
      *
      * * NOTE: SEQNO 1 SHIFTS TO SEQNO 2 WHEN REPO IS REDEEMED TO SAVE
      *         THE REPO INFO, NO INFO CHANGES...IF REVERSED, IT THEN GOES
      *         BACK TO SEQNO 1.
      *
      * REV:
      *  MJD 092995 ADDED LSX-NET-BAL-DUE AND LSX-DEFICIENCY-BAL.
      *  MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  BAH 010615 ADDED LSX-AUCTION-EXPENSE FOR REGACC PR# 1478
      *  BAH 020625 ADDED LSX-REASON-CODE FOR REGACC PR# 1775
      *  BLV 020718 ADDED A NUMBER OF NEW FIELDS, DETAILED COLLATERAL
      *             INFO, STORAGE ADDRESS, 2 PHONE NUMBERS, SOME MISC.
      *             FLAGS, PAGE 4 OF REPO INFO, REGACC PR# 1840
      *  BAH 020815 ADDED LSX-EXTRACTED FOR A REPO EXTRACTION, REGACC PR#1872
      *  BAH 020924 ADDED LSX-TITLE-SENT-DATE, REGACC PR# 2
      *  BAH 160208 SPLIT OUT LSFILE PD0003
      *   CS 190507 ADD FUTURE FIELDS
      *  BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      *  BAH 20230600 ADDED FILLER
      ******************************************************************
       78  LSX-SIZE                       VALUE 1780.
       01  LSX-REC.
           03  LSX1-KEY.
               05  LSX-BRNO             PIC 9(4).
               05  LSX-ACCTNO           PIC 9(8).
               05  LSX-SEQNO            PIC 99.

      $XFD DATE=YYYYMMDD
           03  LSX-LTOUCH-DATE          PIC 9(8)     COMP-3.

      $XFD DATE=YYYYMMDD
           03  LSX-REPODATE             PIC 9(8)      COMP-3.
           03  LSX-TITLE-MILEAGE        PIC 9(6)      COMP-3.
           03  LSX-VOLUNTARY-FG         PIC X.
           03  LSX-PERFECT              PIC X.
      $XFD DATE=YYYYMMDD
           03  LSX-AUTHORIZED-DATE      PIC 9(8)      COMP-3.
           03  LSX-AUTHORIZED-BY        PIC X(16).
           03  LSX-REASON               PIC X(60).

           03  LSX-X-ASSIGNED-TO        PIC X(30).
      $XFD DATE=YYYYMMDD
           03  LSX-ASSIGNED-DATE        PIC 9(8)      COMP-3.

           03  LSX-REPO-CKNO            PIC 9(6)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSX-REPO-CKDATE          PIC 9(8)      COMP-3.
           03  LSX-REPO-CKAMT           PIC S9(6)V99  COMP-3.

           03  LSX-STORED-ADR1          PIC X(30).
      $XFD DATE=YYYYMMDD
           03  LSX-STORED-CKDATE        PIC 9(8)      COMP-3.
           03  LSX-STORED-CKNO          PIC 9(6)      COMP-3.
           03  LSX-STORED-CKAMT         PIC S9(6)V99  COMP-3.

      $XFD DATE=YYYYMMDD
           03  LSX-REDEEM-DATE          PIC 9(8)      COMP-3.
           03  LSX-BUREAU-NOTIFIED      PIC X(10).
      $XFD DATE=YYYYMMDD
           03  LSX-BUREAU-DATE          PIC 9(8)      COMP-3.
           03  LSX-BUREAU-BY            PIC X(10).
      $XFD DATE=YYYYMMDD
           03  LSX-10DAY-DATE           PIC 9(8)      COMP-3.
           03  LSX-10DAY-BY             PIC X(10).
           03  LSX-10DAY-COST           PIC S9(6)V99  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSX-TITLE-APPL-DATE      PIC 9(8)      COMP-3.
           03  LSX-TITLE-APPL-BY        PIC X(10).
           03  LSX-TITLE-APPL-COST      PIC S9(6)V99  COMP-3.

           03  LSX-COND-MILEAGE         PIC 9(6)      COMP-3.
           03  LSX-COND-REPORT-Y-N      PIC X.
           03  LSX-REPAIRS-DESC         PIC X(60)     OCCURS 2.
           03  LSX-REPAIRS-COST         PIC S9(6)V99  COMP-3.

           03  LSX-ASSIGN-TO-TBL      OCCURS 2.
               05  LSX-ASSIGN-TO            PIC X(30).
      $XFD DATE=YYYYMMDD
               05  LSX-ASSIGN-COMPLETE-DATE PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
               05  LSX-ASSIGN-CKDATE        PIC 9(8)      COMP-3.
               05  LSX-ASSIGN-CKNO          PIC 9(6)      COMP-3.
               05  LSX-ASSIGN-CKAMT         PIC S9(6)V99  COMP-3.
 
           03  LSX-OTH-EXP-TBL        OCCURS 3.
               05  LSX-OTH-EXP          PIC X(30).
      $XFD DATE=YYYYMMDD
               05  LSX-OTH-EXP-CKDATE   PIC 9(8)      COMP-3.
               05  LSX-OTH-EXP-CKNO     PIC 9(6)      COMP-3.
               05  LSX-OTH-EXP-CKAMT    PIC S9(6)V99  COMP-3.

      $XFD DATE=YYYYMMDD
           03  LSX-INS-RB-REQ-DATE      PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSX-INS-RB-REC-DATE      PIC 9(8)      COMP-3.
           03  LSX-INS-RB-AMT           PIC S9(6)V99  COMP-3.

      $XFD DATE=YYYYMMDD
           03  LSX-RB-REQ-DATE          PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSX-RB-REC-DATE          PIC 9(8)      COMP-3.
           03  LSX-RB-AMT               PIC S9(6)V99  COMP-3.

      $XFD DATE=YYYYMMDD
           03  LSX-SIC-INS-FILE-DATE    PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSX-SIC-INS-PAID-DATE    PIC 9(8)      COMP-3.
           03  LSX-SIC-INS-PAID-AMT     PIC S9(6)V99  COMP-3.

           03  LSX-BOOK-VALUE           PIC 9(6)V99   COMP-3.

           03  LSX-SOLD-TO              PIC X(30)     OCCURS 3.
      $XFD DATE=YYYYMMDD
           03  LSX-SOLD-TO-DATE         PIC 9(8)      COMP-3.
           03  LSX-SOLD-TO-AMT          PIC S9(6)V99  COMP-3.

           03  LSX-DLNO                 PIC 9(4)V99   COMP-3.
           03  LSX-DLR-RSV-CKNO         PIC 9(6)      COMP-3.
           03  LSX-DLR-RSV-CKAMT        PIC S9(6)V99  COMP-3.

           03  LSX-PL-EXPENSE           PIC S9(6)V99  COMP-3.

           03  LSX-DEFICIENCY-Y-N       PIC X.
           03  LSX-NET-BAL-DUE          PIC S9(7)V99  COMP-3.
           03  LSX-DEFICIENCY-BAL       PIC S9(7)V99  COMP-3.

           03  LSX-AUCTION-EXPENSE      PIC S9(7)V99  COMP-3.

           03  LSX-REASON-CODE          PIC XX.

           03  LSX-TRIM                 PIC X(3).
           03  LSX-DOORS                PIC X.
           03  LSX-COLOR                PIC X(10).
           03  LSX-CYLINDERS            PIC 99.
           03  LSX-AUTO-TRANS           PIC X.
           03  LSX-AC                   PIC X.
           03  LSX-SROOF                PIC X.
           03  LSX-LEATHER              PIC X.
           03  LSX-4WD                  PIC X.
           03  LSX-DIESEL               PIC X.
           03  LSX-DUAL-AIR             PIC X.
           03  LSX-EXT-CAB              PIC X.
           03  LSX-AGENT-PHONE          PIC 9(10) COMP-3.
           03  LSX-AGENT-FAX            PIC 9(10) COMP-3.
           03  LSX-STORED-ADR2          PIC X(24).
           03  LSX-X-STORED-CITY        PIC X(15).
           03  LSX-X-STORED-STATE       PIC XX.
           03  LSX-X-STORED-ZIP         PIC X(10).
           03  LSX-INS-CLAIM-PENDING    PIC X.
           03  LSX-WARRANTY-AVAILABLE   PIC X.
           03  LSX-INS-REBATE-REQUESTED PIC X.
           03  LSX-EXTRACTED            PIC X.
      $XFD DATE=YYYYMMDD
           03  LSX-TITLE-SENT-DATE      PIC 9(8)     COMP-3.

      * THIS IS THE START OF WHAT USED TO BE THE "Y" RECORD IN A15
      * LSY
           03  LSX-DISPOSAL-HOLD        PIC XX.
      $XFD DATE=YYYYMMDD
           03  LSX-NEXT-HLD-RVW-DATE    PIC 9(8)     COMP-3.
           03  LSX-CASE-ID              PIC 9(10)    COMP-3.
           03  LSX-COLLATERAL-LOCATION  PIC XX.
           03  LSX-VIN-MATCH            PIC X.
           03  LSX-PERSONALS            PIC X.
           03  LSX-KEYS                 PIC X.
           03  LSX-DRIVES               PIC X.
           03  LSX-DMG                  PIC X.
           03  LSX-Y-ASSIGNED-TO        PIC X(40).
           03  LSX-SUPPLIER-ID          PIC 9(7)     COMP-3.
           03  LSX-TRANSPORT-EMAIL      PIC X(40).
           03  LSX-STORED-LOCATION      PIC X(40).
           03  LSX-STORED-ADDRESS       PIC X(40).
           03  LSX-Y-STORED-CITY        PIC X(20).
           03  LSX-Y-STORED-STATE       PIC X(2).
           03  LSX-Y-STORED-ZIP         PIC X(10).
           03  LSX-AGENCY-NAME          PIC X(40).
           03  LSX-AGENCY-ADDRESS       PIC X(40).
           03  LSX-AGENCY-CITY          PIC X(20).
           03  LSX-AGENCY-STATE         PIC X(2).
           03  LSX-AGENCY-ZIP           PIC X(10).
           03  LSX-AGENCY-PHONE         PIC 9(10)    COMP-3.

           03  FILLER                 PIC X(530).
