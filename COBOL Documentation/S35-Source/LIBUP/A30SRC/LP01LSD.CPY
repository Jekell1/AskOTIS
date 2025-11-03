      * COPYMEMBER: LIBLP/LP01LSD
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : 
      *
      *    DEFICIENCY LETTER INFO
      *
      * REV:
      *  BAH 050818 ADDED "D" RECORD TYPE FOR DEFICIENCY LETTER INFO, REGACC
      *             REGACC PR# 112
      *  BAH 060724 ADDED LS-D-DEF-LETTER-SENT, REGACC PR# 138
      *  BAH 130227 ADDED MORE LS-D FIELDS FOR REGACC PR# 333
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      ******************************************************************
       78  LSD-SIZE                       VALUE 800.
       01  LSD-REC.
           03  LSD1-KEY.
               05  LSD-BRNO               PIC 9(4).
               05  LSD-ACCTNO             PIC 9(8).
               05  LSD-SEQNO              PIC 99.

      *>>XDD DATE=YYYYMMDD
           03  LSD-LTOUCH-DATE            PIC 9(8)     COMP-3.

           03  LSD-DEFICIENCY-AREA.
      *>>XDD DATE=YYYYMMDD
               05  LSD-STOLEN-DATE           PIC 9(8)     COMP-3.
               05  LSD-GAP-INS-PAID          PIC S9(7)V99 COMP-3.
               05  LSD-OTHER-EXPENSE         PIC S9(7)V99 COMP-3.
               05  LSD-MISC-AMT-EXPENSE      PIC S9(7)V99 COMP-3.
               05  LSD-MISC-DESC-EXPENSE     PIC X(15).
               05  LSD-TOT-EXPENSE           PIC S9(7)V99 COMP-3.
               05  LSD-INT-REB               PIC S9(7)V99 COMP-3.
               05  LSD-CPP-REB               PIC S9(7)V99 COMP-3.
               05  LSD-GAP-REB               PIC S9(7)V99 COMP-3.
               05  LSD-WARRANTY-REB          PIC S9(7)V99 COMP-3.
               05  LSD-MISC-AMT-CREDIT       PIC S9(7)V99 COMP-3.
               05  LSD-MISC-DESC-CREDIT      PIC X(15).
               05  LSD-TOT-CREDITS           PIC S9(7)V99 COMP-3.

               05  LSD-PAID-TO-OTHER         PIC S9(7)V99 COMP-3.
               05  LSD-AMT-FROM-OWING        PIC S9(7)V99 COMP-3.
               05  LSD-AMT-OF-SURPLUS        PIC S9(7)V99 COMP-3.
               05  LSD-AMT-OF-DEFICIENCY     PIC S9(7)V99 COMP-3.
               05  LSD-SEND-FMNO             PIC 9(4)     COMP-3.

               05  LSD-INSURANCE-PROCEEDS    PIC S9(7)V99 COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSD-DATE-CHECK-RECV       PIC 9(8)     COMP-3.
               05  LSD-INSURANCE-CHECK       PIC S9(7)V99 COMP-3.
               05  LSD-WHO-ADJUSTED          PIC X(20).
               05  LSD-INS-COMPANY           PIC X(20).
      *>>XDD DATE=YYYYMMDD
               05  LSD-DEF-LETTER-SENT-DATE  PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSD-SALES-DATE            PIC 9(8)     COMP-3.
               05  LSD-AUCTION               PIC X(24).
               05  LSD-AUCTN-TIME            PIC 9(4)     COMP-3.
               05  LSD-AUCTN-ADR             PIC X(24).
               05  LSD-AUCTN-CITY            PIC X(15).
               05  LSD-AUCTN-STATE           PIC X(2).
               05  LSD-AUCTN-ZIP             PIC X(5).
               05  LSD-AUCTN-PHONE           PIC 9(10)    COMP-3.

               05  LSD-REPO-CHARGE           PIC S9(5)V99 COMP-3.
               05  LSD-STORAGE-CHARGE        PIC S9(5)V99 COMP-3.
               05  LSD-REPAIR-CHARGE         PIC S9(5)V99 COMP-3.
               05  LSD-OTHER-CHARGE          PIC S9(5)V99 COMP-3.
               05  LSD-STORAGE-RATE          PIC S9(5)V99 COMP-3.

               05  LSD-EST-REBATE            PIC S9(5)V99 COMP-3.
               05  LSD-MINIMUM-BID           PIC S9(5)V99 COMP-3.
               05  LSD-MAY-REINSTATE         PIC X.
               05  LSD-MAY-NOT-REINSTATE     PIC X.
               05  LSD-IN-PAST-12MONTHS      PIC X.
               05  LSD-2X-DURING-CONTRACT    PIC X.
               05  LSD-OTHER                 PIC X.
               05  LSD-OTHER-COMMENTS        PIC X(60).
               05  FILLER REDEFINES LSD-OTHER-COMMENTS.
                   07  LSD-OTHER-COMMENTS1   PIC X(30).
                   07  LSD-OTHER-COMMENTS2   PIC X(30).
               05  LSD-CA-PYMTS-AS-AGREED    PIC X.
               05  LSD-CA-KEEP-INS           PIC X.
               05  LSD-CA-KEEP-LIEN-FREE     PIC X.
               05  LSD-CA-OTHER              PIC X.
               05  LSD-CA-OTHER-DESC         PIC X(30).
               05  LSD-CURE                  PIC X(33).

               05  FILLER                    PIC X(369).

