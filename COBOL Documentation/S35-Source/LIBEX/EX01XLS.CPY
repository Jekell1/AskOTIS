      * COPYMEMBER: LIBEX/EX01XLS
      **************************************************************************
      * REPO TRAILER EXTRACTION FILE          1522 BYTES
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      * BLV 020822 ADDED NEW FIELDS FROM REPO PAGE 4, REGACC PR# 1840
      * BAH 020924 ADDED LS-X-TITLE-SENT-DATE PAGE 4, REGACC PR# 2
      * BLV 021004 FIXED FIELDS I ACCIDENTLY LEFT AS COMP-6 (FROM PR# 1840)
      *  CS 040903 ADDED XLS-REGION-NUMBER [PR#76 REGACC]
      * BAH 130227 ADDED NEW LS-Y FIELDS TO END OF REPO, REGACC PR# 333
      **************************************************************************
       78  XLS-SIZE                      VALUE 1522.
       01  XLS-REC.
           03  XLS-BRACCTNO              PIC 9(10).
           03  XLS-ACCTNO                PIC 9(08).
           03  XLS-CODE                  PIC X(01).
           03  XLS-SEQNO                 PIC 9(01).
           03  XLS-REPODATE              PIC 9(08).
           03  XLS-TITLE-MILEAGE         PIC 9(06).
           03  XLS-VOLUNTARY-FG          PIC X(01).
           03  XLS-PERFECT               PIC X(01).
           03  XLS-AUTHORIZED-DATE       PIC 9(08).
           03  XLS-AUTHORIZED-BY         PIC X(16).
           03  XLS-REASON                PIC X(60).
           03  XLS-ASSIGNED-TO           PIC X(30).
           03  XLS-ASSIGNED-DATE         PIC 9(08).
           03  XLS-REPO-CKNO             PIC 9(06).
           03  XLS-REPO-CKDATE           PIC 9(08).
           03  XLS-REPO-CKAMT            PIC +9(6).99.
           03  XLS-STORED-ADR1           PIC X(30).
           03  XLS-STORED-CKDATE         PIC 9(08).
           03  XLS-STORED-CKNO           PIC 9(06).
           03  XLS-STORED-CKAMT          PIC +9(6).99.
           03  XLS-REDEEM-DATE           PIC 9(08).
           03  XLS-BUREAU-NOTIFIED       PIC X(10).
           03  XLS-BUREAU-DATE           PIC 9(08).
           03  XLS-BUREAU-BY             PIC X(10).
           03  XLS-10DAY-DATE            PIC 9(08).
           03  XLS-10DAY-BY              PIC X(10).
           03  XLS-10DAY-COST            PIC +9(06).99.
           03  XLS-TITLE-APPL-DATE       PIC 9(08).
           03  XLS-TITLE-APPL-BY         PIC X(10).
           03  XLS-TITLE-APPL-COST       PIC +9(06).99.
           03  XLS-COND-MILEAGE          PIC 9(06).
           03  XLS-COND-REPORT-Y-N       PIC X(01).
           03  XLS-REPAIRS-DESC1         PIC X(60).
           03  XLS-REPAIRS-DESC2         PIC X(60).
           03  XLS-REPAIRS-COST          PIC +9(06).99.
           03  XLS-ASSIGN-TO1            PIC X(30).
           03  XLS-ASSIGN-COMPLETE1      PIC 9(08).
           03  XLS-ASSIGN-CKDATE1        PIC 9(08).
           03  XLS-ASSIGN-CKNO1          PIC 9(06).
           03  XLS-ASSIGN-CKAMT1         PIC +9(06).99.
           03  XLS-ASSIGN-TO2            PIC X(30).
           03  XLS-ASSIGN-COMPLETE2      PIC 9(08).
           03  XLS-ASSIGN-CKDATE2        PIC 9(08).
           03  XLS-ASSIGN-CKNO2          PIC 9(06).
           03  XLS-ASSIGN-CKAMT2         PIC +9(06).99.
           03  XLS-OTH-EXP1              PIC X(30).
           03  XLS-OTH-EXP-CKDATE1       PIC 9(08).
           03  XLS-OTH-EXP-CKNO1         PIC 9(06).
           03  XLS-OTH-EXP-CKAMT1        PIC +9(06).99.
           03  XLS-OTH-EXP2              PIC X(30).
           03  XLS-OTH-EXP-CKDATE2       PIC 9(08).
           03  XLS-OTH-EXP-CKNO2         PIC 9(06).
           03  XLS-OTH-EXP-CKAMT2        PIC +9(06).99.
           03  XLS-OTH-EXP3              PIC X(30).
           03  XLS-OTH-EXP-CKDATE3       PIC 9(08).
           03  XLS-OTH-EXP-CKNO3         PIC 9(06).
           03  XLS-OTH-EXP-CKAMT3        PIC +9(06).99.
           03  XLS-INS-RB-REQ-DATE       PIC 9(08).
           03  XLS-INS-RB-REC-DATE       PIC 9(08).
           03  XLS-INS-RB-AMT            PIC +9(6).99.
           03  XLS-RB-REQ-DATE           PIC 9(08).
           03  XLS-RB-REC-DATE           PIC 9(08).
           03  XLS-RB-AMT                PIC +9(06).99.
           03  XLS-SIC-INS-FILE-DATE     PIC 9(08).
           03  XLS-SIC-INS-PAID-DATE     PIC 9(08).
           03  XLS-SIC-INS-PAID-AMT      PIC +9(06).99.
           03  XLS-BOOK-VALUE            PIC 9(06).99.
           03  XLS-SOLD-TO1              PIC X(30).
           03  XLS-SOLD-TO2              PIC X(30).
           03  XLS-SOLD-TO3              PIC X(30).
           03  XLS-SOLD-TO-DATE          PIC 9(08).
           03  XLS-SOLD-TO-AMT           PIC +9(06).99.
           03  XLS-DLNO                  PIC 9(04).99.
           03  XLS-DLR-RSV-CKNO          PIC 9(06).
           03  XLS-DLR-RSV-CKAMT         PIC +9(06).99.
           03  XLS-PL-EXPENSE            PIC +9(06).99.
           03  XLS-DEFICIENCY-Y-N        PIC X(01).
           03  XLS-NET-BAL-DUE           PIC +9(07).99.
           03  XLS-DEFICIENCY-BAL        PIC +9(07).99.
           03  XLS-AUCTION-EXPENSE       PIC +9(07).99.
           03  XLS-REASON-CODE           PIC XX.
           03  XLS-TRIM                  PIC X(3).
           03  XLS-DOORS                 PIC X.
           03  XLS-COLOR                 PIC X(10).
           03  XLS-CYLINDERS             PIC 99.
           03  XLS-AUTO-TRANS            PIC X.
           03  XLS-AC                    PIC X.
           03  XLS-SROOF                 PIC X.
           03  XLS-LEATHER               PIC X.
           03  XLS-4WD                   PIC X.
           03  XLS-DIESEL                PIC X.
           03  XLS-DUAL-AIR              PIC X.
           03  XLS-EXT-CAB               PIC X.
           03  XLS-AGENT-PHONE           PIC 9(10).
           03  XLS-AGENT-FAX             PIC 9(10).
           03  XLS-STORED-ADR2           PIC X(24).
           03  XLS-STORED-CITY           PIC X(15).
           03  XLS-STORED-STATE          PIC XX.
           03  XLS-STORED-ZIP            PIC X(10).
           03  XLS-INS-CLAIM-PENDING     PIC X.
           03  XLS-WARRANTY-AVAILABLE    PIC X.
           03  XLS-INS-REBATE-REQUESTED  PIC X.
           03  XLS-EXTRACTED             PIC X.
           03  XLS-TITLE-SENT-DATE       PIC 9(8).
           03  XLS-REGION-NUMBER         PIC 9(2).

           03  YLS-DISPOSAL-HOLD         PIC XX.
           03  YLS-NEXT-HLD-RVW-DATE     PIC 9(8).
           03  YLS-CASE-ID               PIC 9(10).
           03  YLS-COLLATERAL-LOCATION   PIC XX.
           03  YLS-VIN-MATCH             PIC X.
           03  YLS-PERSONALS             PIC X.
           03  YLS-KEYS                  PIC X.
           03  YLS-DRIVES                PIC X.
           03  YLS-DMG                   PIC X.
           03  YLS-ASSIGNED-TO           PIC X(40).
           03  YLS-SUPPLIER-ID           PIC 9(7).
           03  YLS-TRANSPORT-EMAIL       PIC X(40).
           03  YLS-STORED-LOCATION       PIC X(40).
           03  YLS-STORED-ADDRESS        PIC X(40).
           03  YLS-STORED-CITY           PIC X(20).
           03  YLS-STORED-STATE          PIC X(2).
           03  YLS-STORED-ZIP            PIC X(10).
           03  YLS-AGENCY-NAME           PIC X(40).
           03  YLS-AGENCY-ADDRESS        PIC X(40).
           03  YLS-AGENCY-CITY           PIC X(20).
           03  YLS-AGENCY-STATE          PIC X(2).
           03  YLS-AGENCY-ZIP            PIC X(10).
           03  YLS-AGENCY-PHONE          PIC 9(10).

           03  XLS-UNUSED                PIC X(48).
           03  XLS-CR                    PIC X(01).
           03  XLS-LF                    PIC X(01).
