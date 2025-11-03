      * COPYMEMBER: LIBLP/LP01LSX
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : (1800) BYTES
      *
      *       REPOSSESION INFO RECORD
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
      ******************************************************************
       78  LSX-SIZE                       VALUE 1800.
       01  LSX-REC.
           03  LSX1-KEY.
               05  LSX-BRNO              PIC 9(4).
               05  LSX-ACCTNO            PIC 9(8).
               05  LSX-SEQNO             PIC 99.

      *>>XDD DATE=YYYYMMDD
           03  LSX-LTOUCH-DATE               PIC 9(8)     COMP-3.

           03  LSX-REPOSSESION-AREA.
      *>>XDD DATE=YYYYMMDD
               05  LSX-REPODATE              PIC 9(8)      COMP-3.
               05  LSX-TITLE-MILEAGE         PIC 9(6)      COMP-3.
               05  LSX-VOLUNTARY-FG          PIC X.
               05  LSX-PERFECT               PIC X.
      *>>XDD DATE=YYYYMMDD
               05  LSX-AUTHORIZED-DATE       PIC 9(8)      COMP-3.
               05  LSX-AUTHORIZED-BY         PIC X(16).
               05  LSX-REASON                PIC X(60).

               05  LSX-X-ASSIGNED-TO         PIC X(30).
      *>>XDD DATE=YYYYMMDD
               05  LSX-ASSIGNED-DATE         PIC 9(8)      COMP-3.

               05  LSX-REPO-CKNO             PIC 9(6)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSX-REPO-CKDATE           PIC 9(8)      COMP-3.
               05  LSX-REPO-CKAMT            PIC S9(6)V99  COMP-3.

               05  LSX-STORED-ADR1           PIC X(30).
      *>>XDD DATE=YYYYMMDD
               05  LSX-STORED-CKDATE         PIC 9(8)      COMP-3.
               05  LSX-STORED-CKNO           PIC 9(6)      COMP-3.
               05  LSX-STORED-CKAMT          PIC S9(6)V99  COMP-3.

      *>>XDD DATE=YYYYMMDD
               05  LSX-REDEEM-DATE           PIC 9(8)      COMP-3.

               05  LSX-BUREAU-NOTIFIED       PIC X(10).
      *>>XDD DATE=YYYYMMDD
               05  LSX-BUREAU-DATE           PIC 9(8)      COMP-3.
               05  LSX-BUREAU-BY             PIC X(10).

      *>>XDD DATE=YYYYMMDD
               05  LSX-10DAY-DATE            PIC 9(8)      COMP-3.
               05  LSX-10DAY-BY              PIC X(10).
               05  LSX-10DAY-COST            PIC S9(6)V99  COMP-3.

      *>>XDD DATE=YYYYMMDD
               05  LSX-TITLE-APPL-DATE       PIC 9(8)      COMP-3.
               05  LSX-TITLE-APPL-BY         PIC X(10).
               05  LSX-TITLE-APPL-COST       PIC S9(6)V99  COMP-3.

               05  LSX-COND-MILEAGE          PIC 9(6)      COMP-3.
               05  LSX-COND-REPORT-Y-N       PIC X.
               05  LSX-REPAIRS-DESC         OCCURS 2
                                              PIC X(60).
               05  LSX-REPAIRS-COST          PIC S9(6)V99  COMP-3.

               05  LSX-ASSIGN-TO-TBL        OCCURS 2.
                   07  LSX-ASSIGN-TO            PIC X(30).
      *>>XDD DATE=YYYYMMDD
                   07  LSX-ASSIGN-COMPLETE-DATE PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
                   07  LSX-ASSIGN-CKDATE        PIC 9(8)      COMP-3.
                   07  LSX-ASSIGN-CKNO          PIC 9(6)      COMP-3.
                   07  LSX-ASSIGN-CKAMT         PIC S9(6)V99  COMP-3.

               05  LSX-OTH-EXP-TBL          OCCURS 3.
                   07  LSX-OTH-EXP           PIC X(30).
      *>>XDD DATE=YYYYMMDD
                   07  LSX-OTH-EXP-CKDATE    PIC 9(8)      COMP-3.
                   07  LSX-OTH-EXP-CKNO      PIC 9(6)      COMP-3.
                   07  LSX-OTH-EXP-CKAMT     PIC S9(6)V99  COMP-3.

      *>>XDD DATE=YYYYMMDD
               05  LSX-INS-RB-REQ-DATE       PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSX-INS-RB-REC-DATE       PIC 9(8)      COMP-3.
               05  LSX-INS-RB-AMT            PIC S9(6)V99  COMP-3.

      *>>XDD DATE=YYYYMMDD
               05  LSX-RB-REQ-DATE           PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSX-RB-REC-DATE           PIC 9(8)      COMP-3.
               05  LSX-RB-AMT                PIC S9(6)V99  COMP-3.

      *>>XDD DATE=YYYYMMDD
               05  LSX-SIC-INS-FILE-DATE     PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSX-SIC-INS-PAID-DATE     PIC 9(8)      COMP-3.
               05  LSX-SIC-INS-PAID-AMT      PIC S9(6)V99  COMP-3.

               05  LSX-BOOK-VALUE            PIC 9(6)V99   COMP-3.

               05  LSX-SOLD-TO              OCCURS 3
                                              PIC X(30).
      *>>XDD DATE=YYYYMMDD
               05  LSX-SOLD-TO-DATE          PIC 9(8)      COMP-3.
               05  LSX-SOLD-TO-AMT           PIC S9(6)V99  COMP-3.

               05  LSX-DLNO                  PIC 9(4)V99   COMP-3.
               05  LSX-DLR-RSV-CKNO          PIC 9(6)      COMP-3.
               05  LSX-DLR-RSV-CKAMT         PIC S9(6)V99  COMP-3.

               05  LSX-PL-EXPENSE            PIC S9(6)V99  COMP-3.

               05  LSX-DEFICIENCY-Y-N        PIC X.
               05  LSX-NET-BAL-DUE           PIC S9(7)V99  COMP-3.
               05  LSX-DEFICIENCY-BAL        PIC S9(7)V99  COMP-3.

               05  LSX-AUCTION-EXPENSE       PIC S9(7)V99  COMP-3.

               05  LSX-REASON-CODE           PIC XX.

               05  LSX-TRIM                  PIC X(3).
               05  LSX-DOORS                 PIC X.
               05  LSX-COLOR                 PIC X(10).
               05  LSX-CYLINDERS             PIC 99.
               05  LSX-AUTO-TRANS            PIC X.
               05  LSX-AC                    PIC X.
               05  LSX-SROOF                 PIC X.
               05  LSX-LEATHER               PIC X.
               05  LSX-4WD                   PIC X.
               05  LSX-DIESEL                PIC X.
               05  LSX-DUAL-AIR              PIC X.
               05  LSX-EXT-CAB               PIC X.
               05  LSX-AGENT-PHONE           PIC 9(10) COMP-3.
               05  LSX-AGENT-FAX             PIC 9(10) COMP-3.
               05  LSX-STORED-ADR2           PIC X(24).
               05  LSX-X-STORED-CITY         PIC X(15).
               05  LSX-X-STORED-STATE        PIC XX.
               05  LSX-X-STORED-ZIP          PIC X(10).
               05  LSX-INS-CLAIM-PENDING     PIC X.
               05  LSX-WARRANTY-AVAILABLE    PIC X.
               05  LSX-INS-REBATE-REQUESTED  PIC X.
               05  LSX-EXTRACTED             PIC X.
      *>>XDD DATE=YYYYMMDD
               05  LSX-TITLE-SENT-DATE       PIC 9(8)     COMP-3.

      * THIS IS THE START OF WHAT USED TO BE THE "Y" RECORD IN A15
      * LSY
               05  LSX-DISPOSAL-HOLD         PIC XX.
      *>>XDD DATE=YYYYMMDD
               05  LSX-NEXT-HLD-RVW-DATE     PIC 9(8)     COMP-3.
               05  LSX-CASE-ID               PIC 9(10)    COMP-3.
               05  LSX-COLLATERAL-LOCATION   PIC XX.
               05  LSX-VIN-MATCH             PIC X.
               05  LSX-PERSONALS             PIC X.
               05  LSX-KEYS                  PIC X.
               05  LSX-DRIVES                PIC X.
               05  LSX-DMG                   PIC X.
               05  LSX-Y-ASSIGNED-TO         PIC X(40).
               05  LSX-SUPPLIER-ID           PIC 9(7)     COMP-3.
               05  LSX-TRANSPORT-EMAIL       PIC X(40).
               05  LSX-STORED-LOCATION       PIC X(40).
               05  LSX-STORED-ADDRESS        PIC X(40).
               05  LSX-Y-STORED-CITY           PIC X(20).
               05  LSX-Y-STORED-STATE          PIC X(2).
               05  LSX-Y-STORED-ZIP            PIC X(10).
               05  LSX-AGENCY-NAME           PIC X(40).
               05  LSX-AGENCY-ADDRESS        PIC X(40).
               05  LSX-AGENCY-CITY           PIC X(20).
               05  LSX-AGENCY-STATE          PIC X(2).
               05  LSX-AGENCY-ZIP            PIC X(10).
               05  LSX-AGENCY-PHONE          PIC 9(10)    COMP-3.

               05  FILLER                     PIC X(550).
