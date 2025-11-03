      * COPYMEMBER: LIBLP/LPCERNW

      ******************************************************************
      *    CASH EARNINGS CALCULATION WORKERS
      *    NOTE:
      *         THE   PICTURES OF THE 'EARN-SV-....' WORKERS
      *         MUST MATCH THEIR SP-FILE COUNTER PART.
      *
      * REV:
      *  JTG 010689 MADE EARNINGS 6 LEVELS AND ADDED CERN
      *  JTG 051089 CHANGED WORKERS TO COMP
      *  JTG 091189 ADDED WKDATES FOR FORMULA 6
      *  JTG 123091 ADDED CERN-ORIG AND CERN-PRES-VALUE
      *             FOR SP-ERNFRMLA = 11
      *  JTG 052493 REVISED FOR BALLOONS
      *  JTG 012794 ADDED CERN-SAVE-EARN-INT & CERN-SAVE
      *                     FOR ERNFRMLA = 13 DISCOUNT YIELD - INT
      *  JTG 072794 ADDED CERN-EARNINGS-OWING
      *                   CERN-EARNINGS-ERNINT
      *                   CERN-EARNINGS-PR-INT FOR,
      *             SP-ERNFRMLA = 15, REAL INTEREST BEARING METHOD
      *                                               - INSTANT AUTO, KY
      *             SP-ERNFRMLA = 16, REAL INTEREST BEARING METHOD
      *             DISCOUNT EARNINGS A % OF PRINCIPAL PAID   FAIRCO, FL
      *             ADDED CERN-RECASTPRIN WORKER
      *               "   CERN-RECASTACCERN WORKER
      *               "   CERN-TOTINT        INPUT FIELD
      *  JTG        ADDED FIELDS FOR SP-ERNFRMLA 12 RE: MIDSTREAMS
      *  JTG        ADDED FIELD FOR FIX MODE CERN-FIX-MODE-FG
      *  JFF 052896 ADDED CERN-PRINBAL FOR SP-ERNFRMLA = 20
      *  JTG 032798 ADDED CERN-TRCD, RE: ERNFRMLA = 6, PREMIER FINANCE
      *  JTG 042198 ADDED CERN-SET, RE: REWRITE OF ERNFRMLA = 6, TO WORK
      *             WITH NON MONTHLY PAPER
      *  JTG 010522 ADDED CERN-APINT, CERN-APLC, CERN-APOTH, CERN-APOT2
      *             NEW CODE '23' FOR SP-ERNFRMLA                 PAACO PR#1469
      *  JTG 020123 EXPANDED PAYMNTD FIELDS TO 1,000,000        LENDMARK PR#J021
      *  JTG 050718 ADDED NEW FORMULAS FOR MEXICO
      *             SP-MFFRMLA-05 SP-RBFRMLA2 (G) SP-ERNFRMLA (24)
      *             ADDED CERN-LN-INT-EARN CERN-LN-SC-EARN
      *                   CERN-LN-MF-EARN CERN-LN-DF-EARN            WORLD #0408
      *  BAH 2023.1109 CHANGED CERN-SV-REBTBL FROM 40 TO 65 TO MATCH SPR
      **************************************************************************
       01  CERNINGS-WORKERS.
      * INPUT FIELDS:
           03  CERN-APCUR             PIC S9(7)V99   COMP.
           03  CERN-APINT             PIC S9(7)V99   COMP.
           03  CERN-APLC              PIC S9(7)V99   COMP.
           03  CERN-PAYMNTD           PIC 9(7)V99    COMP.
           03  CERN-TOTLCHG           PIC 9(6)V99    COMP.
           03  CERN-TOTINT            PIC 9(6)V99    COMP.
           03  CERN-PAYDATE           PIC 9(8)       COMP.
           03  CERN                   PIC 9          COMP VALUE 0.
           03  CERN-PRINBAL           PIC S9(6)V99   COMP.
           03  CERN-FIX-MODE-FG       PIC X               VALUE " ".
           03  CERN-TRCD              PIC XX              VALUE " ".
           03  CERN-LN-INT-EARN       PIC S9(6)V99   COMP.
           03  CERN-LN-SC-EARN        PIC S9(6)V99   COMP.

      * OUTPUT FIELDS:
           03  CERN-EARNINGS          OCCURS 6 TIMES
                                      PIC S9(7)V99   COMP.
           03  CERN-EARNINGS-OWING    PIC S9(7)V99   COMP.
           03  CERN-EARNINGS-ERNINT   PIC S9(7)V99   COMP.
           03  CERN-EARNINGS-PR-INT   PIC S9(7)V99   COMP.

      * WORK FIELDS:
           03  CERN-SAVE              PIC 9          VALUE 0.
           03  CERN-SET               PIC 9          VALUE 0.
           03  CERN-PRES-VALUE        PIC S9(7)V99   COMP-3.
           03  CERN-SAVE-PAYMNTD      PIC 9(7)V99    COMP.
           03  CERN-EARN              PIC S9(7)V99   COMP.
           03  CERN-SAVE-INT-EARN     PIC S9(7)V99   COMP.
           03  CERN-PAYMENTS          PIC 9(5)V9(8).
           03  CERN-PAYMENTSX      REDEFINES CERN-PAYMENTS.
               05  CERN-WPAYS         PIC 9(5).
               05  CERN-PPAYS         PIC V9(8).
           03  CERN-SAVE-WPAYS        PIC 9(5) COMP.
           03  CERN-WORKER            PIC S9(7)V9(8) COMP.
           03  CERN-WORKNO            PIC S9(5)      COMP.
           03  CERN-SUMW              PIC S9(5)      COMP.
           03  CERN-SUMW1             PIC S9(5)      COMP.
           03  CERN-DAYS              PIC S9(5)      COMP.
           03  CERN-WKDATE1           PIC 9(8)       COMP.
           03  CERN-WKDATE2           PIC 9(8)       COMP.
           03  CERN-SAVE-WKDATE1      PIC 9(8)       COMP.
           03  CERN-MDTE-DATE         PIC 9(8)       COMP.
           03  CERN-MDTE-FLAG         PIC X.
           03  CERN-MDTE-ORGTERM-FG   PIC X.

      *    NOTE:
      *         THE      PICTURES OF THE 'CERN-SV-....' WORKERS
      *         MUST MATCH THEIR SP-FILE COUNTER PART.
           03  CERN-SV-YEARTYPE       PIC 9(4)       COMP.
           03  CERN-SV-REBTBL         PIC X(65).
           03  CERN-SV-DISFRMLA       PIC X.

           03  CERN-PDAYS             PIC 9(3)       COMP.

           03  CERN-SV-APRATE           PIC 999V999    COMP.
           03  CERN-SV-1STPYAMT         PIC 9(7)V99    COMP.
           03  CERN-SV-REGPYAMT         PIC 9(7)V99    COMP.
           03  CERN-SV-LASTPYAMT        PIC 9(7)V99    COMP.
           03  CERN-SV-NOREGPY          PIC 9(3)       COMP.
           03  CERN-SV-INTDATE          PIC 9(8)       COMP.
           03  CERN-SV-LOANDATE         PIC 9(8)       COMP.
           03  CERN-SV-1STPYDATE        PIC 9(8)       COMP.
           03  CERN-SV-ORGTERM          PIC 999        COMP.
           03  CERN-SV-ANTICTERM        PIC 999        COMP.

      * WORK LOAN FIELDS:
           03  CERN-WK-LOANDATE         PIC 9(8)         COMP.
           03  CERN-WK-INTDATE          PIC 9(8)         COMP.
           03  CERN-WK-PURDATE          PIC 9(8)         COMP.
           03  CERN-WK-ORIG-1STPYDATE   PIC 9(8)         COMP.
           03  CERN-WK-1STPYDATE        PIC 9(8)         COMP.
           03  CERN-WK-ORGTERM          PIC 9(3)         COMP.
           03  CERN-WK-SUM-ORGTERM      PIC 9(3)         COMP.
           03  CERN-WK-1STPYAMT         PIC 9(7)V99      COMP.
           03  CERN-WK-NOREGPY          PIC 9(3)         COMP.
           03  CERN-WK-REGPYAMT         PIC 9(7)V99      COMP.
           03  CERN-WK-LASTPYAMT        PIC 9(7)V99      COMP.
           03  CERN-WK-ORIGPURAMT       PIC 9(7)V99      COMP.
           03  CERN-WK-ANTICERN         PIC S9(6)V99     COMP.
           03  CERN-WK-ANTICADJ         PIC S9(6)V99     COMP.
           03  CERN-WK-ACCERN           PIC S9(6)V99     COMP.
           03  CERN-WK-UFERN            PIC S9(6)V99     COMP.
           03  CERN-WK-FINCHG           PIC 9(7)V99      COMP.
           03  CERN-WK-ANTICTERM        PIC S999         COMP.
           03  CERN-WK-TOTNODEF         PIC S999         COMP.
           03  CERN-WK-APRATE           PIC 999V999      COMP.
           03  CERN-WK-FINANCED         PIC 9(7)V99      COMP.
           03  CERN-WK-REBATE           PIC X.
           03  CERN-WK-DISFRMLA         PIC X.
           03  CERN-WK-INCLUDE-DEF-FG   PIC X.
