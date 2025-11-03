      * COPYMEMBER: LIBEX/EX01XLSD
      ******************************************************************
      *    LOAN STATUS DEFICIENCY TRAILER
      *
      * REV:
      *  BAH 050908 ADDED "D" RECORD TYPE FOR DEFICIENCY LETTER INFO, REGACC
      *             REGACC PR# 112
      *   CS 070912 ADDED "D" FIELDS FOR REINSTATMENT STATUS
      *   CS 070920 PER KEVIN, ON AUCTION TIME NEED AN AM/PM INDICATOR
      *   CS 070925 PER KEVIN, REMOVE XLS-D-SALES-PRICE PR#180 REGACC
      *  BAH 130227 ADDED NEW LS-D FIELDS, REGACC PR# 333
      ******************************************************************
       78  XLSD-SIZE                     VALUE 721.
       01  XLSD-REC.
           03  XLS-D-BRACCTNO             PIC 9(10).
           03  XLS-D-ACCTNO               PIC 9(8).
           03  XLS-D-CODE                 PIC X.
           03  XLS-D-SEQNO                PIC 9.

           03  XLS-D-STOLEN-DATE           PIC 9(8).
           03  XLS-D-GAP-INS-PAID          PIC +9(7).99.
           03  XLS-D-OTHER-EXPENSE         PIC +9(7).99.
           03  XLS-D-MISC-AMT-EXPENSE      PIC +9(7).99.
           03  XLS-D-MISC-DESC-EXPENSE     PIC X(15).
           03  XLS-D-TOT-EXPENSE           PIC +9(7).99.
           03  XLS-D-INT-REB               PIC +9(7).99.
           03  XLS-D-CPP-REB               PIC +9(7).99.
           03  XLS-D-GAP-REB               PIC +9(7).99.
           03  XLS-D-WARRANTY-REB          PIC +9(7).99.
           03  XLS-D-MISC-AMT-CREDIT       PIC +9(7).99.
           03  XLS-D-MISC-DESC-CREDIT      PIC X(15).
           03  XLS-D-TOT-CREDITS           PIC +9(7).99.

           03  XLS-D-PAID-TO-OTHER         PIC +9(7).99.
           03  XLS-D-AMT-FROM-OWING        PIC +9(7).99.
           03  XLS-D-AMT-OF-SURPLUS        PIC +9(7).99.
           03  XLS-D-AMT-OF-DEFICIENCY     PIC +9(7).99.
           03  XLS-D-SEND-FMNO             PIC 9(3).

           03  XLS-D-INSURANCE-PROCEEDS    PIC +9(7)V99.
           03  XLS-D-DATE-CHECK-RECV       PIC 9(8).
           03  XLS-D-INSURANCE-CHECK       PIC +9(7)V99.
           03  XLS-D-WHO-ADJUSTED          PIC X(20).
           03  XLS-D-INS-COMPANY           PIC X(20).

           03  XLS-D-REGION-NO             PIC 9(2).
      *REINSTATEMENT FIELDS
           03  XLS-D-SALES-DATE          PIC 9(8).
           03  XLS-D-AUCTION             PIC X(24).
           03  XLS-D-AUCTN-TIME          PIC 9(4).
           03  XLS-D-AUCTN-TIME-AMPM     PIC X(2).
           03  XLS-D-AUCTN-ADR           PIC X(24).
           03  XLS-D-AUCTN-CITY          PIC X(15).
           03  XLS-D-AUCTN-STATE         PIC X(2).
           03  XLS-D-AUCTN-ZIP           PIC X(5).
           03  XLS-D-AUCTN-PHONE         PIC 9(10).
           03  XLS-D-REPO-CHARGE         PIC +9(5).99.
           03  XLS-D-STORAGE-CHARGE      PIC +9(5).99.
           03  XLS-D-REPAIR-CHARGE       PIC +9(5).99.
           03  XLS-D-OTHER-CHARGE        PIC +9(5).99.
           03  XLS-D-STORAGE-RATE        PIC +9(5).99.
           03  XLS-D-EST-REBATE          PIC +9(5).99.
           03  XLS-D-MINIMUM-BID         PIC +9(5).99.
           03  XLS-D-MAY-REINSTATE       PIC X.
           03  XLS-D-MAY-NOT-REINSTATE   PIC X.
           03  XLS-D-IN-PAST-12MONTHS    PIC X.
           03  XLS-D-2X-DURING-CONTRACT  PIC X.
           03  XLS-D-OTHER               PIC X.
           03  XLS-D-OTHER-COMMENTS      PIC X(60). 
           03  FILLER REDEFINES XLS-D-OTHER-COMMENTS.
               05  XLS-D-OTHER-COMMENTS1     PIC X(30).
               05  XLS-D-OTHER-COMMENTS2     PIC X(30).

      *    CALCULATED FIELDS
           03  XLS-D-CALC-PAST-DUE-PRIN       PIC +9(7).99.
           03  XLS-D-CALC-INT-DUE             PIC +9(5).99.
           03  XLS-D-CALC-LC-DUE              PIC +9(5).99.
           03  XLS-D-CALC-TOT-TO-REINSTATE    PIC +9(7).99.
           03  XLS-D-CALC-PMTS-X-2            PIC +9(5).99.
           03  XLS-D-CALC-NXDUE-AFT-REPO      PIC 9(8).
           03  XLS-D-CALC-NXT-SCHEDULED-AMT   PIC +9(7).99.
           03  XLS-D-CALC-NXTDUE-LC-AFT-REPO  PIC 9(8).
           03  XLS-D-CALC-REDEEM-AMT          PIC +9(7).99.

           03  XLS-D-CA-PYMTS-AS-AGREED  PIC X.
           03  XLS-D-CA-KEEP-INS         PIC X.
           03  XLS-D-CA-KEEP-LIEN-FREE   PIC X.
           03  XLS-D-CA-OTHER            PIC X.
           03  XLS-D-CA-OTHER-DESC       PIC X(30).
           03  XLS-D-CURE                PIC X(33).

           03  FILLER                      PIC X(58).

           03  XLS-D-CR                    PIC X.
           03  XLS-D-LF                    PIC X.
