      * COPYMEMBER: LIBLP/LP01LP_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN PAYMENT FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0504 ***NEW***
      * JKC 2022-0915 ADDED QLP1-WHERE-SCAN-BEG-KEY.
      * BAH 20240212 ADDED QLP-REPAY-TRANS-ID  PIC 9(10).
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  LP1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LP1-CURSOR-STAT-GOOD              VALUE "00".
           88  LP1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{NOT GREATER CURSOR FOR START-NOT-GREATER & READ-PREVIOUS}
       01  LP1-CURSOR-NG-STAT           PIC X(2) VALUE SPACES.
           88  LP1-CURSOR-NG-STAT-GOOD           VALUE "00".
           88  LP1-CURSOR-NG-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{SCAN CURSOR FOR START-SCAN  }
       01  LP1-CURSOR-SC-STAT           PIC X(2) VALUE SPACES.
           88  LP1-CURSOR-SC-STAT-GOOD           VALUE "00".
           88  LP1-CURSOR-SC-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LP1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLP1-WHERE-BEG-KEY.
           03  QLP1-WBEG-BRNO             PIC 9(04).
           03  QLP1-WBEG-ACCTNO           PIC 9(08).
           03  QLP1-WBEG-SEQNO            PIC 9(05).
 
       01  QLP1-WHERE-END-KEY.
           03  QLP1-WEND-BRNO             PIC 9(04).
           03  QLP1-WEND-ACCTNO           PIC 9(08).
           03  QLP1-WEND-SEQNO            PIC 9(05).
 
       01  QLP1-WHERE-SCAN-BEG-KEY.
           03  QLP1-WSBEG-BRNO            PIC 9(04).
           03  QLP1-WSBEG-ACCTNO          PIC 9(08).
      *--> 03  QLP1-WSBEG-SEQNO           PIC 9(05). ***NOT NEEDED***
 
       01  QLP-REC.
           03  QLP-BRNO             PIC  9(4).
           03  QLP-ACCTNO           PIC  9(8).
           03  QLP-SEQNO            PIC  9(5).
           03  QLP-LTOUCH-DATE          PIC X(10).
           03  QLP-USERID               PIC X(10).
           03  QLP-POSTDATE             PIC X(10).
           03  QLP-POSTTIME             PIC 9(6).
           03  QLP-TRDATE               PIC X(10).
           03  QLP-PAYDATE              PIC X(10).
           03  QLP-TRCD                 PIC XX.
           03  QLP-REFNO                PIC X(5).
           03  QLP-TRAMT                PIC S9(7)V99.
           03  QLP-APOTH                PIC S9(7)V99.
           03  QLP-APOT2                PIC S9(7)V99.
           03  QLP-APINT                PIC S9(7)V99.
           03  QLP-APLC                 PIC S9(7)V99.
           03  QLP-APCUR                PIC S9(7)V99.
           03  QLP-OTHBAL               PIC S9(7)V99.
           03  QLP-OT2BAL               PIC S9(7)V99.
           03  QLP-INTBAL               PIC S9(7)V99.
           03  QLP-LCBAL                PIC S9(7)V99.
           03  QLP-CURBAL               PIC S9(7)V99.
           03  QLP-INTDUE               PIC S9(7)V99.
           03  QLP-DLPROC               PIC S9(7)V99.
           03  QLP-EARNED-1             PIC S9(7)V99.
           03  QLP-EARNED-2             PIC S9(7)V99.
           03  QLP-EARNED-3             PIC S9(7)V99.
           03  QLP-INTPAID              PIC S9(7)V99.
           03  QLP-SSNO                 PIC 9(10).
           03  QLP-PDTH-DATE            PIC X(10).
           03  QLP-LCPDTH-DATE          PIC X(10).
           03  QLP-INTPDTH-DATE         PIC X(10).
           03  QLP-TILLNO               PIC 99.
           03  QLP-APINTOWE             PIC S9(7)V99.
           03  QLP-DLPARVHELD-ADJ       PIC S9(7)V99.
           03  QLP-LT-SEQNO             PIC 9(3).
           03  QLP-IBPC                 PIC X.
           03  QLP-MANUAL-RB            PIC X.
           03  QLP-INSTYPES             PIC X.
           03  QLP-REV                  PIC X.
           03  QLP-REVTRCD              PIC XX.
           03  QLP-REVSEQNO             PIC 9(5).
           03  QLP-STATUSFG             PIC X.
           03  QLP-NEWSTAT-RATE         PIC 999V999.
           03  QLP-LCPARTIALS           PIC S9(7)V99.
           03  QLP-ADDON-ORIG-REGPY     PIC S9(7)V99.
           03  QLP-NEW1ST-PYDATE        PIC X(10).
           03  QLP-DEF-DUEDATE          PIC X(10).
           03  QLP-LCPAID               PIC S9(7)V99.
           03  QLP-OLD1ST-PYDATE        PIC X(10).
           03  QLP-DEF-TERM             PIC 9(3).
           03  QLP-ADDON-LAST-PYAMT     PIC S9(7)V99.
           03  QLP-DLRVCHGS-ADJ-1       PIC S9(7)V99.
           03  QLP-DLRVCHGS-ADJ-2       PIC S9(7)V99.
           03  QLP-DLRVCHGS-ADJ-3       PIC S9(7)V99.
           03  QLP-DLPARVPAID-ADJ       PIC S9(7)V99.
           03  QLP-DLNO                  PIC 9(4)V99.
           03  QLP-PREPAY-PENALTY        PIC S9(7)V99.
           03  QLP-PRORATED-INS-REB      PIC S9(7)V99.
           03  QLP-POSTING-BR            PIC 9(4).
           03  QLP-NOLCHG                PIC S9(3).
           03  QLP-COLLECTOR             PIC X(3).
           03  QLP-EXTRA-PRIN-LFP        PIC S9(7)V99.
           03  QLP-REMAIN-INT-SINCE-LFP  PIC S9(7)V99.
           03  QLP-REMAIN-PRIN-SINCE-LFP PIC S9(7)V99.
           03  QLP-APPLIED-TO-ESCROW     PIC S9(7)V99.
           03  QLP-ESCROW-PREPAID        PIC S9(7)V99.
           03  QLP-PRIOR-POOLID          PIC 9(5).
           03  QLP-ADDON-INTPDTH-DATE    PIC X(10).
           03  QLP-FROMCL                PIC  999.
           03  QLP-TOCL                  PIC  999.
           03  QLP-ALT-PREPAY-FG         PIC X.
           03  QLP-REPAY-TRANS-ID        PIC 9(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
