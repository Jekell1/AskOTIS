      * COPYMEMBER: LIBLP/LP01LSD_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN STATUS -  DEFICIENCY LETTER
      *
      *=================================================================
      * REV:
      * AMM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LSD1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LSD1-CURSOR-STAT-GOOD             VALUE "00".
           88  LSD1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LSD1-RETURN-CODE                PIC S9(4)   COMP-5.
 
       01  QLSD1-WHERE-BEG-KEY.
           03  QLSD1-WBEG-BRNO             PIC 9(04).
           03  QLSD1-WBEG-ACCTNO           PIC 9(08).
           03  QLSD1-WBEG-SEQNO            PIC 9(02).

       01  QLSD1-WHERE-END-KEY.
           03  QLSD1-WEND-BRNO             PIC 9(04).
           03  QLSD1-WEND-ACCTNO           PIC 9(08).
           03  QLSD1-WEND-SEQNO            PIC 9(02).
 
       01  QLSD-REC.
           03  QLSD-BRNO               PIC 9(4).
           03  QLSD-ACCTNO             PIC 9(8).
           03  QLSD-SEQNO              PIC 99.
           03  QLSD-LTOUCH-DATE           PIC X(10).
           03  QLSD-STOLEN-DATE           PIC X(10).
           03  QLSD-GAP-INS-PAID          PIC S9(7)V99.
           03  QLSD-OTHER-EXPENSE         PIC S9(7)V99.
           03  QLSD-MISC-AMT-EXPENSE      PIC S9(7)V99.
           03  QLSD-MISC-DESC-EXPENSE     PIC X(15).
           03  QLSD-TOT-EXPENSE           PIC S9(7)V99.
           03  QLSD-INT-REB               PIC S9(7)V99.
           03  QLSD-CPP-REB               PIC S9(7)V99.
           03  QLSD-GAP-REB               PIC S9(7)V99.
           03  QLSD-WARRANTY-REB          PIC S9(7)V99.
           03  QLSD-MISC-AMT-CREDIT       PIC S9(7)V99.
           03  QLSD-MISC-DESC-CREDIT      PIC X(15).
           03  QLSD-TOT-CREDITS           PIC S9(7)V99.
           03  QLSD-PAID-TO-OTHER         PIC S9(7)V99.
           03  QLSD-AMT-FROM-OWING        PIC S9(7)V99.
           03  QLSD-AMT-OF-SURPLUS        PIC S9(7)V99.
           03  QLSD-AMT-OF-DEFICIENCY     PIC S9(7)V99.
           03  QLSD-SEND-FMNO             PIC 9(4).
           03  QLSD-INSURANCE-PROCEEDS    PIC S9(7)V99.
           03  QLSD-DATE-CHECK-RECV       PIC X(10).
           03  QLSD-INSURANCE-CHECK       PIC S9(7)V99.
           03  QLSD-WHO-ADJUSTED          PIC X(20).
           03  QLSD-INS-COMPANY           PIC X(20).
           03  QLSD-DEF-LETTER-SENT-DATE  PIC X(10).
           03  QLSD-SALES-DATE            PIC X(10).
           03  QLSD-AUCTION               PIC X(24).
           03  QLSD-AUCTN-TIME            PIC 9(4).
           03  QLSD-AUCTN-ADR             PIC X(24).
           03  QLSD-AUCTN-CITY            PIC X(15).
           03  QLSD-AUCTN-STATE           PIC X(2).
           03  QLSD-AUCTN-ZIP             PIC X(5).
           03  QLSD-AUCTN-PHONE           PIC 9(10).
           03  QLSD-REPO-CHARGE           PIC S9(5)V99.
           03  QLSD-STORAGE-CHARGE        PIC S9(5)V99.
           03  QLSD-REPAIR-CHARGE         PIC S9(5)V99.
           03  QLSD-OTHER-CHARGE          PIC S9(5)V99.
           03  QLSD-STORAGE-RATE          PIC S9(5)V99.
           03  QLSD-EST-REBATE            PIC S9(5)V99.
           03  QLSD-MINIMUM-BID           PIC S9(5)V99.
           03  QLSD-MAY-REINSTATE         PIC X.
           03  QLSD-MAY-NOT-REINSTATE     PIC X.
           03  QLSD-IN-PAST-12MONTHS      PIC X.
           03  QLSD-2X-DURING-CONTRACT    PIC X.
           03  QLSD-OTHER                 PIC X.
           03  QLSD-OTHER-COMMENTS1       PIC X(30).
           03  QLSD-OTHER-COMMENTS2       PIC X(30).
           03  QLSD-CA-PYMTS-AS-AGREED    PIC X.
           03  QLSD-CA-KEEP-INS           PIC X.
           03  QLSD-CA-KEEP-LIEN-FREE     PIC X.
           03  QLSD-CA-OTHER              PIC X.
           03  QLSD-CA-OTHER-DESC         PIC X(30).
           03  QLSD-CURE                  PIC X(33).

       EXEC SQL END DECLARE SECTION END-EXEC.
