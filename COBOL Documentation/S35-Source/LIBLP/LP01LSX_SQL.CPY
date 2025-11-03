      * COPYMEMBER: LIBLP/LP01LSX_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN STATUS -  REPOSSESSION
      *
      *=================================================================
      * REV:
      * AMM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LSX1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LSX1-CURSOR-STAT-GOOD             VALUE "00".
           88  LSX1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  LSX1-CURSOR-NG-STAT          PIC X(2) VALUE SPACES.
           88  LSX1-CURSOR-NG-STAT-GOOD          VALUE "00".
           88  LSX1-CURSOR-NG-STAT-BAD           VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LSX1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLSX1-WHERE-BEG-KEY.
           03  QLSX1-WBEG-BRNO             PIC 9(04).
           03  QLSX1-WBEG-ACCTNO           PIC 9(08).
           03  QLSX1-WBEG-SEQNO            PIC 9(02).

       01  QLSX1-WHERE-END-KEY.
           03  QLSX1-WEND-BRNO             PIC 9(04).
           03  QLSX1-WEND-ACCTNO           PIC 9(08).
           03  QLSX1-WEND-SEQNO            PIC 9(02).

       01  QLSX-REC.
           03  QLSX-BRNO             PIC 9(4).
           03  QLSX-ACCTNO           PIC 9(8).
           03  QLSX-SEQNO            PIC 99.
           03  QLSX-LTOUCH-DATE          PIC X(10).
           03  QLSX-REPODATE             PIC X(10).
           03  QLSX-TITLE-MILEAGE        PIC 9(6).
           03  QLSX-VOLUNTARY-FG         PIC X.
           03  QLSX-PERFECT              PIC X.
           03  QLSX-AUTHORIZED-DATE      PIC X(10).
           03  QLSX-AUTHORIZED-BY        PIC X(16).
           03  QLSX-REASON               PIC X(60).
           03  QLSX-X-ASSIGNED-TO        PIC X(30).
           03  QLSX-ASSIGNED-DATE        PIC X(10).
           03  QLSX-REPO-CKNO            PIC 9(6).
           03  QLSX-REPO-CKDATE          PIC X(10).
           03  QLSX-REPO-CKAMT           PIC S9(6)V99.
           03  QLSX-STORED-ADR1          PIC X(30).
           03  QLSX-STORED-CKDATE        PIC X(10).
           03  QLSX-STORED-CKNO          PIC 9(6).
           03  QLSX-STORED-CKAMT         PIC S9(6)V99.
           03  QLSX-REDEEM-DATE          PIC X(10).
           03  QLSX-BUREAU-NOTIFIED      PIC X(10).
           03  QLSX-BUREAU-DATE          PIC X(10).
           03  QLSX-BUREAU-BY            PIC X(10).
           03  QLSX-10DAY-DATE           PIC X(10).
           03  QLSX-10DAY-BY             PIC X(10).
           03  QLSX-10DAY-COST           PIC S9(6)V99.
           03  QLSX-TITLE-APPL-DATE      PIC X(10).
           03  QLSX-TITLE-APPL-BY        PIC X(10).
           03  QLSX-TITLE-APPL-COST      PIC S9(6)V99.
           03  QLSX-COND-MILEAGE         PIC 9(6).
           03  QLSX-COND-REPORT-Y-N      PIC X.
           03  QLSX-REPAIRS-DESC-1       PIC X(60).
           03  QLSX-REPAIRS-DESC-2       PIC X(60).
           03  QLSX-REPAIRS-COST         PIC S9(6)V99.
           03  QLSX-ASSIGN-TO-1            PIC X(30).
           03  QLSX-ASSIGN-COMPLETE-DATE-1 PIC X(10).
           03  QLSX-ASSIGN-CKDATE-1        PIC X(10).
           03  QLSX-ASSIGN-CKNO-1          PIC 9(6).
           03  QLSX-ASSIGN-CKAMT-1         PIC S9(6)V99.
           03  QLSX-ASSIGN-TO-2            PIC X(30).
           03  QLSX-ASSIGN-COMPLETE-DATE-2 PIC X(10).
           03  QLSX-ASSIGN-CKDATE-2        PIC X(10).
           03  QLSX-ASSIGN-CKNO-2          PIC 9(6).
           03  QLSX-ASSIGN-CKAMT-2         PIC S9(6)V99.
           03  QLSX-OTH-EXP-1          PIC X(30).
           03  QLSX-OTH-EXP-CKDATE-1   PIC X(10).
           03  QLSX-OTH-EXP-CKNO-1     PIC 9(6).
           03  QLSX-OTH-EXP-CKAMT-1    PIC S9(6)V99.
           03  QLSX-OTH-EXP-2          PIC X(30).
           03  QLSX-OTH-EXP-CKDATE-2   PIC X(10).
           03  QLSX-OTH-EXP-CKNO-2     PIC 9(6).
           03  QLSX-OTH-EXP-CKAMT-2    PIC S9(6)V99.
           03  QLSX-OTH-EXP-3          PIC X(30).
           03  QLSX-OTH-EXP-CKDATE-3   PIC X(10).
           03  QLSX-OTH-EXP-CKNO-3     PIC 9(6).
           03  QLSX-OTH-EXP-CKAMT-3    PIC S9(6)V99.
           03  QLSX-INS-RB-REQ-DATE      PIC X(10).
           03  QLSX-INS-RB-REC-DATE      PIC X(10).
           03  QLSX-INS-RB-AMT           PIC S9(6)V99.
           03  QLSX-RB-REQ-DATE          PIC X(10).
           03  QLSX-RB-REC-DATE          PIC X(10).
           03  QLSX-RB-AMT               PIC S9(6)V99.
           03  QLSX-SIC-INS-FILE-DATE    PIC X(10).
           03  QLSX-SIC-INS-PAID-DATE    PIC X(10).
           03  QLSX-SIC-INS-PAID-AMT     PIC S9(6)V99.
           03  QLSX-BOOK-VALUE           PIC 9(6)V99.
           03  QLSX-SOLD-TO-1            PIC X(30).
           03  QLSX-SOLD-TO-2            PIC X(30).
           03  QLSX-SOLD-TO-3            PIC X(30).
           03  QLSX-SOLD-TO-DATE         PIC X(10).
           03  QLSX-SOLD-TO-AMT          PIC S9(6)V99.
           03  QLSX-DLNO                 PIC 9(4)V99.
           03  QLSX-DLR-RSV-CKNO         PIC 9(6).
           03  QLSX-DLR-RSV-CKAMT        PIC S9(6)V99.
           03  QLSX-PL-EXPENSE           PIC S9(6)V99.
           03  QLSX-DEFICIENCY-Y-N       PIC X.
           03  QLSX-NET-BAL-DUE          PIC S9(7)V99.
           03  QLSX-DEFICIENCY-BAL       PIC S9(7)V99.
           03  QLSX-AUCTION-EXPENSE      PIC S9(7)V99.
           03  QLSX-REASON-CODE          PIC XX.
           03  QLSX-TRIM                 PIC X(3).
           03  QLSX-DOORS                PIC X.
           03  QLSX-COLOR                PIC X(10).
           03  QLSX-CYLINDERS            PIC 99.
           03  QLSX-AUTO-TRANS           PIC X.
           03  QLSX-AC                   PIC X.
           03  QLSX-SROOF                PIC X.
           03  QLSX-LEATHER              PIC X.
           03  QLSX-4WD                  PIC X.
           03  QLSX-DIESEL               PIC X.
           03  QLSX-DUAL-AIR             PIC X.
           03  QLSX-EXT-CAB              PIC X.
           03  QLSX-AGENT-PHONE          PIC 9(10).
           03  QLSX-AGENT-FAX            PIC 9(10).
           03  QLSX-STORED-ADR2          PIC X(24).
           03  QLSX-X-STORED-CITY        PIC X(15).
           03  QLSX-X-STORED-STATE       PIC XX.
           03  QLSX-X-STORED-ZIP         PIC X(10).
           03  QLSX-INS-CLAIM-PENDING    PIC X.
           03  QLSX-WARRANTY-AVAILABLE   PIC X.
           03  QLSX-INS-REBATE-REQUESTED PIC X.
           03  QLSX-EXTRACTED            PIC X.
           03  QLSX-TITLE-SENT-DATE      PIC X(10).
           03  QLSX-DISPOSAL-HOLD        PIC XX.
           03  QLSX-NEXT-HLD-RVW-DATE    PIC X(10).
           03  QLSX-CASE-ID              PIC 9(10).
           03  QLSX-COLLATERAL-LOCATION  PIC XX.
           03  QLSX-VIN-MATCH            PIC X.
           03  QLSX-PERSONALS            PIC X.
           03  QLSX-KEYS                 PIC X.
           03  QLSX-DRIVES               PIC X.
           03  QLSX-DMG                  PIC X.
           03  QLSX-Y-ASSIGNED-TO        PIC X(40).
           03  QLSX-SUPPLIER-ID          PIC 9(7).
           03  QLSX-TRANSPORT-EMAIL      PIC X(40).
           03  QLSX-STORED-LOCATION      PIC X(40).
           03  QLSX-STORED-ADDRESS       PIC X(40).
           03  QLSX-Y-STORED-CITY        PIC X(20).
           03  QLSX-Y-STORED-STATE       PIC X(2).
           03  QLSX-Y-STORED-ZIP         PIC X(10).
           03  QLSX-AGENCY-NAME          PIC X(40).
           03  QLSX-AGENCY-ADDRESS       PIC X(40).
           03  QLSX-AGENCY-CITY          PIC X(20).
           03  QLSX-AGENCY-STATE         PIC X(2).
           03  QLSX-AGENCY-ZIP           PIC X(10).
           03  QLSX-AGENCY-PHONE         PIC 9(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
