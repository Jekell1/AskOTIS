      * COPYMEMBER: LIBLP/LP01LS
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : (900) BYTES
      *
      *     CODE   SEQNO
      *        X       1    REPOSSESION INFO RECORD
      *        Y       1    REPOSSESION INFO RECORD - CONTINUED
      *
      *        D       1    DEFICIENCY LETTER INFO
      *
      *        P       1    PAID THRU (DEFERMENT) APPROVAL
      *
      *        F       1    FOLLOW UP EXPANDED INFO 
      *
      *        M       1    MISC FIELDS - UNDEFINED
      *
      *        G       1    GPS STARTER DEVICE 
      *
      *        L       1    LEGAL INFORMATION
      *
      *        R       1    RESCHEDULE/SCRA INFORMATION
      *
      *        J       1    JUDGMENT INFORMATION
      *
      * * NOTE: SEQNO 1 SHIFTS TO SEQNO 2 WHEN REPO IS REDEEMED TO SAVE
      *         THE REPO INFO, NO INFO CHANGES...IF REVERSED, IT THEN GOES
      *         BACK TO SEQNO 1.
      *
      * REV:
      *  MJD 092995 ADDED LS-X-NET-BAL-DUE AND LS-X-DEFICIENCY-BAL.
      *  MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  BAH 010615 ADDED LS-X-AUCTION-EXPENSE FOR REGACC PR# 1478
      *  BAH 020625 ADDED LS-X-REASON-CODE FOR REGACC PR# 1775
      *  BLV 020718 ADDED A NUMBER OF NEW FIELDS, DETAILED COLLATERAL
      *             INFO, STORAGE ADDRESS, 2 PHONE NUMBERS, SOME MISC.
      *             FLAGS, PAGE 4 OF REPO INFO, REGACC PR# 1840
      *  BAH 020815 ADDED LS-X-EXTRACTED FOR A REPO EXTRACTION, REGACC PR#1872
      *  BAH 020924 ADDED LS-X-TITLE-SENT-DATE, REGACC PR# 2
      *  BAH 050818 ADDED "D" RECORD TYPE FOR DEFICIENCY LETTER INFO, REGACC
      *             REGACC PR# 112
      *  BAH 050914 ADDED "P" RECORD TYPE FOR PAID THRU MODIFICATION APPROVAL
      *             WINDOW, LENDMARK PR# 2568
      *  BAH 060724 ADDED LS-D-DEF-LETTER-SENT, REGACC PR# 138
      *  BAH 070308 ADDED "F" FOLLOW UP INFO FOR EXPANDED FIELDS TRITON PR#2937
      *   CS 070827 ADDED FIELDS FOR REPO TRAILER TO LS-D REDEF. ADD FIELDS
      *             LS-SALES-PRICE THRU LS-D-COMMENTS PR#180
      *  BAH 100615 ADDED "M" MISC FIELDS RECORDS FOR UNITED PR# 3615
      *  BAH 110125 ADDED "G" GPS STARTER DEVICE FOR PCCREDIT PEOPLE PR# 3699
      *  BAH 110215 ADDED "L" LEGAL INFORMATION FOR UNITED PR# 3707
      *  BAH 110328 ADDED "R" RESCHEDULE/SCRA INFORMATION  REGACC PR# 289/290
      *  BAH 110803 ADDED LS-L-DATE-CLOSED, LS-L-REASON-CLOSED, UNITED PR# 3847
      *  BAH 120330 ADDED "J" JUDGMENT INFORMATION CITIZENS PR# 3990
      *  BAH 130220 ADDED LS-REPOSSESION-Y-AREA FOR MORE REPO, REGACC PR# 333
      *  BAH 130227 ADDED MORE LS-D FIELDS FOR REGACC PR# 333
      *  BAH 140407 ADDED LS-G-GPS-INSTALLER FOR PEOPLES CREDIT, PR# 4515
      *  BAH 160719 ADDED LS-X-KEYS FOR SOAUTO, LS-Y-KEYS ALREADY HERE BUT
      *             ONLY USED BY REGACC, NOT CHANGING THAT! PR#5133
      *  BAH 2022.0321 ADDED LS-M-TRANS-FROM-BRNO, LS-M-TRANS-FROM-ACCTNO,
      *                LS-M-TRANS-FROM-DATE FOR METRO2 CREDIT BUREAU
      *                "L1" SEGMENT, #1547
      *  BAH 2022.0908 ADDED LS-M-MISPAY-90-DATE FOR VERITEC ILUEXT #1570
      *  BAH 20240109 ADDED SECOND BENEFICIARY INFO TO LS-M #1637
      ******************************************************************
       78  LS-SIZE                       VALUE 900.
       01  LS-REC.
           03  LS1-KEY.
               05  LS-ACCTNO             PIC 9(8).
               05  LS-CODE               PIC X.
               05  LS-SEQNO              PIC 9.

           03  LS-REDEFINE-AREA          PIC X(890).
      *---------------------------------------------------------------
      *    REPOSSESION RECORD "X":
      *---------------------------------------------------------------
           03  LS-REPOSSESION-AREA REDEFINES LS-REDEFINE-AREA.
               05  LS-X-REPODATE              PIC 9(6)      COMP-3.
               05  LS-X-TITLE-MILEAGE         PIC 9(6)      COMP-3.
               05  LS-X-VOLUNTARY-FG          PIC X.
               05  LS-X-PERFECT               PIC X.
               05  LS-X-AUTHORIZED-DATE       PIC 9(6)      COMP-3.
               05  LS-X-AUTHORIZED-BY         PIC X(16).
               05  LS-X-REASON                PIC X(60).
 
               05  LS-X-ASSIGNED-TO           PIC X(30).
               05  LS-X-ASSIGNED-DATE         PIC 9(6).
             
               05  LS-X-REPO-CKNO             PIC 9(6)      COMP-3.
               05  LS-X-REPO-CKDATE           PIC 9(6)      COMP-3.
               05  LS-X-REPO-CKAMT            PIC S9(6)V99  COMP-3.

               05  LS-X-STORED-ADR1           PIC X(30).
               05  LS-X-STORED-CKDATE         PIC 9(6)      COMP-3.
               05  LS-X-STORED-CKNO           PIC 9(6)      COMP-3.
               05  LS-X-STORED-CKAMT          PIC S9(6)V99  COMP-3.

               05  LS-X-REDEEM-DATE           PIC 9(6)      COMP-3.

               05  LS-X-BUREAU-NOTIFIED       PIC X(10).
               05  LS-X-BUREAU-DATE           PIC 9(6)      COMP-3.
               05  LS-X-BUREAU-BY             PIC X(10).

               05  LS-X-10DAY-DATE            PIC 9(6)      COMP-3.
               05  LS-X-10DAY-BY              PIC X(10).
               05  LS-X-10DAY-COST            PIC S9(6)V99  COMP-3.

               05  LS-X-TITLE-APPL-DATE       PIC 9(6)      COMP-3.
               05  LS-X-TITLE-APPL-BY        PIC X(10).
               05  LS-X-TITLE-APPL-COST       PIC S9(6)V99  COMP-3.

               05  LS-X-COND-MILEAGE          PIC 9(6)      COMP-3.
               05  LS-X-COND-REPORT-Y-N       PIC X.
               05  LS-X-REPAIRS-DESC         OCCURS 2
                                              PIC X(60).
               05  LS-X-REPAIRS-COST          PIC S9(6)V99  COMP-3.
      
               05  LS-X-ASSIGN-TO-TBL        OCCURS 2.
                   07  LS-X-ASSIGN-TO         PIC X(30).
                   07  LS-X-ASSIGN-COMPLETE   PIC 9(6)      COMP-3.
                   07  LS-X-ASSIGN-CKDATE     PIC 9(6)      COMP-3.
                   07  LS-X-ASSIGN-CKNO       PIC 9(6)      COMP-3.
                   07  LS-X-ASSIGN-CKAMT      PIC S9(6)V99  COMP-3.

               05  LS-X-OTH-EXP-TBL          OCCURS 3.
                   07  LS-X-OTH-EXP           PIC X(30).
                   07  LS-X-OTH-EXP-CKDATE    PIC 9(6)      COMP-3.
                   07  LS-X-OTH-EXP-CKNO      PIC 9(6)      COMP-3.
                   07  LS-X-OTH-EXP-CKAMT     PIC S9(6)V99  COMP-3.

               05  LS-X-INS-RB-REQ-DATE       PIC 9(6)      COMP-3.
               05  LS-X-INS-RB-REC-DATE       PIC 9(6)      COMP-3.
               05  LS-X-INS-RB-AMT            PIC S9(6)V99  COMP-3.

               05  LS-X-RB-REQ-DATE           PIC 9(6)      COMP-3.
               05  LS-X-RB-REC-DATE           PIC 9(6)      COMP-3.
               05  LS-X-RB-AMT                PIC S9(6)V99  COMP-3.

               05  LS-X-SIC-INS-FILE-DATE     PIC 9(6)      COMP-3.
               05  LS-X-SIC-INS-PAID-DATE     PIC 9(6)      COMP-3.
               05  LS-X-SIC-INS-PAID-AMT      PIC S9(6)V99  COMP-3.

               05  LS-X-BOOK-VALUE            PIC 9(6)V99   COMP-3.

               05  LS-X-SOLD-TO              OCCURS 3
                                              PIC X(30).
               05  LS-X-SOLD-TO-DATE          PIC 9(6)      COMP-3.
               05  LS-X-SOLD-TO-AMT           PIC S9(6)V99  COMP-3.

               05  LS-X-DLNO                  PIC 9(4)V99   COMP-3.
               05  LS-X-DLR-RSV-CKNO          PIC 9(6)      COMP-3.
               05  LS-X-DLR-RSV-CKAMT         PIC S9(6)V99  COMP-3.

               05  LS-X-PL-EXPENSE            PIC S9(6)V99  COMP-3.

               05  LS-X-DEFICIENCY-Y-N        PIC X.
               05  LS-X-NET-BAL-DUE           PIC S9(7)V99  COMP-3.
               05  LS-X-DEFICIENCY-BAL        PIC S9(7)V99  COMP-3.
           
               05  LS-X-AUCTION-EXPENSE       PIC S9(7)V99  COMP-3.

               05  LS-X-REASON-CODE           PIC XX.

               05  LS-X-TRIM                  PIC X(3).
               05  LS-X-DOORS                 PIC X.
               05  LS-X-COLOR                 PIC X(10).
               05  LS-X-CYLINDERS             PIC 99 COMP-6.
               05  LS-X-AUTO-TRANS            PIC X.
               05  LS-X-AC                    PIC X.
               05  LS-X-SROOF                 PIC X.
               05  LS-X-LEATHER               PIC X.
               05  LS-X-4WD                   PIC X.
               05  LS-X-DIESEL                PIC X.
               05  LS-X-DUAL-AIR              PIC X.
               05  LS-X-EXT-CAB               PIC X.
               05  LS-X-AGENT-PHONE           PIC 9(10) COMP-6.
               05  LS-X-AGENT-FAX             PIC 9(10) COMP-6.
               05  LS-X-STORED-ADR2           PIC X(24).
               05  LS-X-STORED-CITY           PIC X(15).
               05  LS-X-STORED-STATE          PIC XX.
               05  LS-X-STORED-ZIP            PIC X(10).
               05  LS-X-INS-CLAIM-PENDING     PIC X.
               05  LS-X-WARRANTY-AVAILABLE    PIC X.
               05  LS-X-INS-REBATE-REQUESTED  PIC X.
               05  LS-X-EXTRACTED             PIC X.
               05  LS-X-TITLE-SENT-DATE       PIC 9(6)      COMP-3.
               05  LS-X-KEYS                  PIC X.
               05  FILLER                     PIC X(17).

      *---------------------------------------------------------------
      *    REPOSSESION RECORD CONTINUED "Y":
      *---------------------------------------------------------------
           03  LS-REPOSSESION-Y-AREA REDEFINES LS-REDEFINE-AREA.
               05  LS-Y-DISPOSAL-HOLD         PIC XX.
               05  LS-Y-NEXT-HLD-RVW-DATE     PIC 9(6)     COMP-6.
               05  LS-Y-CASE-ID               PIC 9(10)    COMP-6.
               05  LS-Y-COLLATERAL-LOCATION   PIC XX.
               05  LS-Y-VIN-MATCH             PIC X.
               05  LS-Y-PERSONALS             PIC X.
               05  LS-Y-KEYS                  PIC X.
               05  LS-Y-DRIVES                PIC X.
               05  LS-Y-DMG                   PIC X.
               05  LS-Y-ASSIGNED-TO           PIC X(40).
               05  LS-Y-SUPPLIER-ID           PIC 9(7).
               05  LS-Y-TRANSPORT-EMAIL       PIC X(40).
               05  LS-Y-STORED-LOCATION       PIC X(40).
               05  LS-Y-STORED-ADDRESS        PIC X(40).
               05  LS-Y-STORED-CITY           PIC X(20).
               05  LS-Y-STORED-STATE          PIC X(2).
               05  LS-Y-STORED-ZIP            PIC X(10).
               05  LS-Y-AGENCY-NAME           PIC X(40).
               05  LS-Y-AGENCY-ADDRESS        PIC X(40).
               05  LS-Y-AGENCY-CITY           PIC X(20).
               05  LS-Y-AGENCY-STATE          PIC X(2).
               05  LS-Y-AGENCY-ZIP            PIC X(10).
               05  LS-Y-AGENCY-PHONE          PIC 9(10) COMP-6.

               05  FILLER                     PIC X(557).



      *---------------------------------------------------------------
      *    DEFICIENCY RECORD "D":
      *---------------------------------------------------------------
           03  LS-DEFICIENCY-AREA REDEFINES LS-REDEFINE-AREA.
               05  LS-D-STOLEN-DATE           PIC 9(6)     COMP-3.
               05  LS-D-GAP-INS-PAID          PIC S9(7)V99 COMP-3.
               05  LS-D-OTHER-EXPENSE         PIC S9(7)V99 COMP-3.
               05  LS-D-MISC-AMT-EXPENSE      PIC S9(7)V99 COMP-3.
               05  LS-D-MISC-DESC-EXPENSE     PIC X(15).
               05  LS-D-TOT-EXPENSE           PIC S9(7)V99 COMP-3.
               05  LS-D-INT-REB               PIC S9(7)V99 COMP-3.
               05  LS-D-CPP-REB               PIC S9(7)V99 COMP-3.
               05  LS-D-GAP-REB               PIC S9(7)V99 COMP-3.
               05  LS-D-WARRANTY-REB          PIC S9(7)V99 COMP-3.
               05  LS-D-MISC-AMT-CREDIT       PIC S9(7)V99 COMP-3.
               05  LS-D-MISC-DESC-CREDIT      PIC X(15).
               05  LS-D-TOT-CREDITS           PIC S9(7)V99 COMP-3.

               05  LS-D-PAID-TO-OTHER         PIC S9(7)V99 COMP-3.
               05  LS-D-AMT-FROM-OWING        PIC S9(7)V99 COMP-3.
               05  LS-D-AMT-OF-SURPLUS        PIC S9(7)V99 COMP-3.
               05  LS-D-AMT-OF-DEFICIENCY     PIC S9(7)V99 COMP-3.
               05  LS-D-SEND-FMNO             PIC 9(3)     COMP-3.

               05  LS-D-INSURANCE-PROCEEDS    PIC S9(7)V99 COMP-3.
               05  LS-D-DATE-CHECK-RECV       PIC 9(6)     COMP-3.
               05  LS-D-INSURANCE-CHECK       PIC S9(7)V99 COMP-3.
               05  LS-D-WHO-ADJUSTED          PIC X(20).
               05  LS-D-INS-COMPANY           PIC X(20).
 
               05  LS-D-DEF-LETTER-SENT       PIC 9(6)    COMP-3.

               05  LS-D-SALES-DATE          PIC 9(6) COMP-6.
               05  LS-D-AUCTION             PIC X(24).
               05  LS-D-AUCTN-TIME          PIC 9(4) COMP-6. 
               05  LS-D-AUCTN-ADR           PIC X(24).
               05  LS-D-AUCTN-CITY          PIC X(15).
               05  LS-D-AUCTN-STATE         PIC X(2).
               05  LS-D-AUCTN-ZIP           PIC X(5).
               05  LS-D-AUCTN-PHONE         PIC 9(10) COMP-6.      

               05  LS-D-REPO-CHARGE         PIC S9(5)V99 COMP-3.
               05  LS-D-STORAGE-CHARGE      PIC S9(5)V99 COMP-3.
               05  LS-D-REPAIR-CHARGE       PIC S9(5)V99 COMP-3.
               05  LS-D-OTHER-CHARGE        PIC S9(5)V99 COMP-3.
               05  LS-D-STORAGE-RATE        PIC S9(5)V99 COMP-3.

               05  LS-D-EST-REBATE          PIC S9(5)V99 COMP-3.
               05  LS-D-MINIMUM-BID         PIC S9(5)V99 COMP-3.
               05  LS-D-MAY-REINSTATE       PIC X.
               05  LS-D-MAY-NOT-REINSTATE   PIC X.
               05  LS-D-IN-PAST-12MONTHS    PIC X.
               05  LS-D-2X-DURING-CONTRACT  PIC X.
               05  LS-D-OTHER               PIC X.
               05  LS-D-OTHER-COMMENTS      PIC X(60).
               05  FILLER REDEFINES LS-D-OTHER-COMMENTS.
                   07  LS-D-OTHER-COMMENTS1     PIC X(30).
                   07  LS-D-OTHER-COMMENTS2     PIC X(30).
               05  LS-D-CA-PYMTS-AS-AGREED  PIC X.
               05  LS-D-CA-KEEP-INS         PIC X.
               05  LS-D-CA-KEEP-LIEN-FREE   PIC X.
               05  LS-D-CA-OTHER            PIC X.
               05  LS-D-CA-OTHER-DESC       PIC X(30).
               05  LS-D-CURE                PIC X(33).

               05  FILLER                   PIC X(486).


      *---------------------------------------------------------------
      *    PAID THRU (DEFERMENT) MODIFICATION APPROVAL  "P":
      *---------------------------------------------------------------
           03  LS-PAID-THRU-AREA REDEFINES LS-REDEFINE-AREA.
               05  LS-P-REASON-CODE           PIC XX.
               05  LS-P-PERIODS-TO-DEFER      PIC 99.
               05  LS-P-REASON-FOR-DEFER      PIC X(50).
               05  LS-P-MORTGAGE-PAST-DUE     PIC X.
               05  LS-P-MORTGAGE-BALANCE      PIC S9(7)V99 COMP-3.
               05  LS-P-ATTEMPT-REFINANCE     PIC X(50).
               05  LS-P-INFO-DATE             PIC 9(6) COMP-3.
               05  LS-P-INFO-USERID           PIC X(10).
               05  LS-P-BR-MANAGER-COMMENTS   PIC X(50).
               05  LS-P-BR-MANAGER-DATE       PIC 9(6) COMP-3.
               05  LS-P-BR-MANAGER-USERID     PIC X(10).
               05  LS-P-AVP-COMMENTS          PIC X(50).
               05  LS-P-AVP-DATE              PIC 9(6) COMP-3.
               05  LS-P-AVP-USERID            PIC X(10).
               05  LS-P-AVP-APPROVE           PIC X.
               05  LS-P-VP-COMMENTS           PIC X(50).
               05  LS-P-VP-DATE               PIC 9(6) COMP-3.
               05  LS-P-VP-USERID             PIC X(10).
               05  LS-P-VP-APPROVE            PIC X.
               05  FILLER                     PIC X(572).

      *---------------------------------------------------------------
      *    FOLLOW UP "F":
      *---------------------------------------------------------------
           03  LS-FOLLOW-UP-AREA REDEFINES LS-REDEFINE-AREA.
               05  LS-F-SKIP-START-DATE       PIC 9(6)      COMP-3.
               05  LS-F-SKIP-END-DATE         PIC 9(6)      COMP-3.
               05  LS-F-OFR-START-DATE        PIC 9(6)      COMP-3.
               05  LS-F-OFR-END-DATE          PIC 9(6)      COMP-3.
               05  LS-F-IMPOUND-START-DATE    PIC 9(6)      COMP-3.
               05  LS-F-IMPOUND-END-DATE      PIC 9(6)      COMP-3.
               05  LS-F-BNKRPT-START-DATE     PIC 9(6)      COMP-3.
               05  LS-F-BNKRPT-END-DATE       PIC 9(6)      COMP-3.
               05  LS-F-INSUR-START-DATE      PIC 9(6)      COMP-3.
               05  LS-F-INSUR-END-DATE        PIC 9(6)      COMP-3.
               05  LS-F-COLLECT-START-DATE    PIC 9(6)      COMP-3.
               05  LS-F-COLLECT-END-DATE      PIC 9(6)      COMP-3.

               05  FILLER                     PIC X(842).

      *---------------------------------------------------------------
      *    GPS STARTER RECORD "G":
      *---------------------------------------------------------------
           03  LS-GPS-AREA REDEFINES LS-REDEFINE-AREA.
               05  LS-G-SERIAL-NO             PIC X(12).
               05  LS-G-STARTER-STAT          PIC X.
               05  LS-G-WARNING-STAT          PIC X.
               05  LS-G-RECOVERY-STAT         PIC X.
               05  LS-G-LAST-COMMAND-DATE     PIC 9(6)      COMP-3.
               05  LS-G-LAST-ADDRESS          PIC X(75).
               05  LS-G-LAST-AUTO-REPORT-DATE PIC 9(6)      COMP-3.
               05  LS-G-GPS-INSTALLER         PIC X(3).
               05  FILLER                     PIC X(789).
      *---------------------------------------------------------------
      *    MISC RECORD "M":
      *---------------------------------------------------------------
           03  LS-MISC-AREA REDEFINES LS-REDEFINE-AREA.
               05  LS-M-MISC-FIELD            PIC X(20) OCCURS 32.

               05  LS-M-TRANS-FROM-BRNO       PIC 9(4).
               05  LS-M-TRANS-FROM-ACCTNO     PIC 9(8).
      * MMDDYY
               05  LS-M-TRANS-FROM-DATE       PIC 9(8).
      * MMDDYY
               05  LS-M-MISPAY-90-DATE        PIC 9(8).
               05  LS-M-SECOND-BEN-NAME       PIC X(32).
               05  LS-M-SECOND-BEN-ADR        PIC X(32).
               05  LS-M-SECOND-BEN-CITY       PIC X(16).
               05  LS-M-SECOND-BEN-STATE      PIC X(2).
               05  LS-M-SECOND-BEN-ZIP        PIC X(10).
               05  LS-M-SECOND-BEN-PHONE      PIC 9(10) COMP-3.
               05  LS-M-SECOND-BEN-RELATION   PIC X(15).

               05  FILLER                     PIC X(109).

      *---------------------------------------------------------------
      *    LEGAL INFORMATION RECORD "L":
      *---------------------------------------------------------------
           03  LS-LEGAL-AREA REDEFINES LS-REDEFINE-AREA.
               05  LS-L-DATE-SUIT-AUTH        PIC 9(6) COMP-3.
               05  LS-L-DATE-SUIT-AUTH-BY     PIC X(20).
               05  LS-L-ATTORNEY-NET-ASSIGN   PIC X(20).
               05  LS-L-LOCAL-ATTORNEY        PIC X(20).
               05  LS-L-ATTORNEY-ADDRESS      PIC X(60).
               05  LS-L-ATTORNEY-PHONE        PIC 9(10) COMP-3.
               05  LS-L-DATE-ASSIGNED         PIC 9(6)  COMP-3.
               05  LS-L-ATTORNEY-ACCTNO       PIC X(20).
               05  LS-L-PRIN-BAL-SENT         PIC S9(7)V99 COMP-3.
               05  LS-L-RECEIVED-ACKNOW       PIC 9(6)  COMP-3.
               05  LS-L-ATTORNEY-LETTER-SENT  PIC 9(6)  COMP-3.
               05  LS-L-HOLD-DATE             PIC 9(6)  COMP-3.
               05  LS-L-RESPONSE-RECEIVED     PIC 9(6)  COMP-3.
               05  LS-L-SUIT-DATE             PIC 9(6)  COMP-3.
               05  LS-L-SUIT-FILED            PIC 9(6)  COMP-3.
               05  LS-L-SERVICE-DATE          PIC 9(6)  COMP-3.
               05  LS-L-CUSTOMER-SERVED       PIC X.
               05  LS-L-ANSWER-FILED          PIC 9(6)  COMP-3.
               05  LS-L-COURT-DATE            PIC 9(6)  COMP-3.
               05  LS-L-JUDGMENT-TYPE         PIC X(20).
               05  LS-L-JUDGMENT-OBTAINED     PIC 9(6)  COMP-3.
               05  LS-L-JUDGMENT-COPY-FILED   PIC X.
               05  LS-L-JUDGMENT-AMT          PIC S9(7)V99 COMP-3.
               05  LS-L-JUDGMENT-PRIN         PIC S9(7)V99 COMP-3.
               05  LS-L-POST-JUDG-INT-RATE    PIC S9(3)V99 COMP-3.
               05  LS-L-JUDG-INT-AMT          PIC S9(7)V99 COMP-3.
               05  LS-L-ATTORNEY-FEES-ALLOWED PIC S9(7)V99 COMP-3.
               05  LS-L-OTHER-FEES-ALLOWED    PIC S9(7)V99 COMP-3.
               05  LS-L-JUDG-INT-RATE         PIC S9(3)V99 COMP-3.
               05  LS-L-ASSET-USED-IN-SUIT    PIC X(20).
               05  LS-L-GARN-FILED-DATE       PIC 9(6)  COMP-3.
               05  LS-L-GARN-TERM-DAYS        PIC 9(3) COMP-3.
               05  LS-L-INTERROGATORY-SENT    PIC 9(6)  COMP-3.
               05  LS-L-INTERROGATORY-ANSW    PIC 9(6)  COMP-3.
               05  LS-L-FIFA-SENT             PIC 9(6)  COMP-3.
               05  LS-L-FIFA-ANSWERED         PIC 9(6)  COMP-3.
               05  LS-L-DATE-TO-RECV-PAY      PIC 9(6)  COMP-3.
               05  LS-L-FOLLOWUP-DATE         PIC 9(6)  COMP-3.
               05  LS-L-FIFA-RENEWED          PIC 9(6)  COMP-3.
               05  LS-L-PAYS-RECV-ATTORNEY    PIC X.
               05  LS-L-DATE-CLOSED           PIC 9(6)  COMP-3.
               05  LS-L-REASON-CLOSED         PIC X(20).
    
               05  FILLER                     PIC X(559).

      *---------------------------------------------------------------
      *    RESCHEDULE/SCRA RECORD "R" 
      *                ^ SERVICE MEMBER CIVIL RELIEF ACT
      *---------------------------------------------------------------
           03  LS-RESCHEDULE-AREA REDEFINES LS-REDEFINE-AREA.
               05  LS-R-SCRA-ACTIVE           PIC X.
               05  LS-R-SCRA-START-DATE       PIC 9(6)      COMP-3.
               05  LS-R-SCRA-END-DATE         PIC 9(6)      COMP-3.
               05  LS-R-SCRA-BORROWER         PIC X.
               05  LS-R-CURRENT-RESCHED-DATE  PIC 9(6)      COMP-3.
               05  LS-R-CURRENT-REASON-CODE   PIC X(5).
               05  LS-R-INITIAL-RESCHED-DATE  PIC 9(6)      COMP-3.
               05  LS-R-INITIAL-REASON-CODE   PIC X(5).
               05  LS-R-INITIAL-TDR-DATE      PIC 9(6)      COMP-3.
               05  LS-R-INITIAL-TDR-REASON    PIC X(5).
               05  LS-R-CURRENT-TDR-DATE      PIC 9(6)      COMP-3.
               05  LS-R-CURRENT-TDR-REASON    PIC X(5).
               05  FILLER                     PIC X(844).

      *---------------------------------------------------------------
      *    JUDGMENT INFORMATION "J"
      *---------------------------------------------------------------
           03  LS-JUDGMENT-AREA REDEFINES LS-REDEFINE-AREA.
               05  LS-J-FILEDATE         PIC 9(6)  COMP-3.
               05  LS-J-CASE-NO          PIC X(15).
               05  LS-J-AMT              PIC S9(7)V99   COMP-3.
               05  LS-J-COUNTY           PIC X(25).
               05  LS-J-CLERK-NAME       PIC X(25).
               05  LS-J-CLERK-ADDR       PIC X(25).
               05  LS-J-CLERK-CITY       PIC X(15).
               05  LS-J-CLERK-STATE      PIC X(2).
               05  LS-J-CLERK-ZIP        PIC X(10).
               05  LS-J-CLERK-PHONE      PIC 9(10) COMP-3.

               05  LS-J-SHERIFF-FEE      PIC S9(7)V99   COMP-3.
               05  LS-J-COURT-FEE        PIC S9(7)V99   COMP-3.
               05  LS-J-ATTY-FEE         PIC S9(7)V99   COMP-3.

               05  LS-J-DISMISS-DATE     PIC 9(6)  COMP-3.
               05  LS-J-GARN-INFO        PIC X(25).
               05  LS-J-EMPLOY-NAME      PIC X(25).
               05  LS-J-EMPLOY-ADDR      PIC X(25).
               05  LS-J-EMPLOY-CITY      PIC X(15).
               05  LS-J-EMPLOY-STATE     PIC X(2).
               05  LS-J-EMPLOY-ZIP       PIC X(10).
               05  LS-J-GARN-DATE        PIC 9(6)  COMP-3.

               05  LS-J-JD-FILED-ON      PIC X.
               05  LS-J-GARN-FILED-ON    PIC X.
               05  LS-J-ATTY-NAME        PIC X(25).
               05  LS-J-ATTY-ADDR        PIC X(25).
               05  LS-J-ATTY-CITY        PIC X(15).
               05  LS-J-ATTY-STATE       PIC X(2).
               05  LS-J-ATTY-ZIP         PIC X(10).
               05  LS-J-ATTY-PHONE       PIC 9(10) COMP-3.

               05  FILLER                PIC X(548).

