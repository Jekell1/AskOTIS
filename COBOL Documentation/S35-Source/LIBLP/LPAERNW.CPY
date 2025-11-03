      * COPYMEMBER: LIBLP/LPAERNW
      **************************************************************************
      *                      (LPAERNW)
      *    EARNINGS (ACCRUAL) CALCULATION WORKERS
      *
      * REV:
      *  JTG 060189 ADDED AERN-ERNDELQ-STOP (PIONEER)
      *  SLC 071189 MADE AERN VALUE 0.
      *  SLC 121289 ADDED NEW WORKERS FOR MIDSTREAM.
      *  JTG 041190 ADDED AERN-SV-DISFRMLA
      *  JTG 081090 ADDED FIELDS FOR ADDON INS.
      *  JTG 081892 ADDED AERN-SAVE-ENDDATE, RE: FRMLA 56
      *  JTG 033195 REMOVED REF: NOODDPY
      *  JTG 082395 ADDED AERN-START-DATE, AERN-TDAYS, AERN-KDAYS
      *                   AERN-I, AERN-I1, AERN-MDTE-DATE
      *  JTG 100695 ADDED AERN-T, AERN-K MERCURY BULK FORMULA
      *  JTG 081798 ADDED AERN-HOLD FIELDS FOR SP-ERNFRMLA 64, MERCURY #E199
      *  JTG 990326 ADDED AERN-LAST-ACCRUAL-DATE AERN-NEW-LAST-ACCRUAL-DATE
      *                   AERN-PROFILE-WORKER
      *             RE: SP-ERNDELQFRMLA 'C' TFC #005
      *  JTG 990513 CREATED AERN-ANTICTERMW WHOLE TERM 9(3)V000000
      *  JTG 020123 EXPANDED PAYMNTD FIELDS TO 1,000,000        LENDMARK PR#J021
      *   CS 121120 ADDED AERN-CERN-FIELDS USED TO RECOMPUTE ACCRUAL 52 EARNINGS
      *             TO CASH.  USES THESE FIELDS TO COME UP WITH A DATE THAT IS
      *             USED INSTEAD OF THE NORMAL MONTH-END DATE WHEN AN ACCOUNT
      *             HITS THE STOP ACCRUAL (SPR) CONTRACTUAL STATUS WORLD PR#828
      *  BAH 2024.0321 REMOVED AERN-DLDISC, AERN-START-DATE, AERN-TDAYS,
      *                AERN-KDAYS, AERN-T, AERN-K, AERN-I, AERN-I1, AERN-PDAYS
      *                AERN-NOREGPY, AERN-LN-1STPYAMT, AERN-LN-REGPYAMT
      **************************************************************************
      * INPUT FIELDS:
       01  AERN-ENDDATE                 PIC S9(8)      COMP.
       01  AERN-LAST-ACCRUAL-DATE       PIC S9(8)      COMP.
       01  AERN                         PIC 9          VALUE 0.
       01  AERN-ADDON-FG                PIC X          VALUE " ".

      * OUTPUT FIELDS:
       01  AERN-EARNINGS-01.
           03  AERN-EARNINGS            OCCURS 6 TIMES
                                        PIC S9(7)V99   COMP-3.
       01  AERN-ERNDELQ-STOP            PIC X.
       01  AERN-NEW-LAST-ACCRUAL-DATE   PIC S9(8)      COMP.

      * WORK FIELDS:
       01  AERNINGS-WORKERS.
           03  AERN-EARN            PIC S9(7)V99      COMP.
           03  AERN-ANTICIPATED     PIC S9(7)V99      COMP.
           03  AERN-ANTICTERM       PIC S9(3)V9(8)    COMP.
           03  AERN-ANTICTERMW      PIC S9(3)         COMP.
           03  AERN-EARN-APR        PIC S9(7)V99      COMP.
           03  AERN-MFEES           PIC S9(3)V99      COMP.
           03  AERN-REMTERM         PIC S9(3)         COMP.
           03  AERN-FLAG1           PIC 9.
           03  AERN-WORK1           PIC S9(7)V99      COMP.
           03  AERN-WORK2           PIC S9(7)V99      COMP.
           03  AERN-PAYSMADE        PIC S999          COMP.
           03  AERN-PAYSDUE         PIC S999          COMP.
           03  AERN-MDTE-DATE       PIC S9(8)         COMP.
           03  AERN-APRATE          PIC S9(3)V9(4)    COMP.
           03  AERN-LNAMT           PIC S9(7)V99      COMP.
           03  AERN-1STPYAMT        PIC 9(7)V99       COMP.
           03  AERN-REGPYAMT        PIC S9(7)V99      COMP.
           03  AERN-LASTPYAMT       PIC 9(7)V99       COMP.
           03  AERN-SAVE-DATE       PIC S9(8)         COMP.
           03  AERN-SAVE-ENDDATE    PIC S9(8)         COMP.
           03  AERN-63-INT-EARN     PIC S9(7)V99      COMP   VALUE 0.

           03  AERN-SV-YEARTYPE     PIC 9(4)          COMP.
           03  AERN-SV-REBTBL       PIC X(40).
           03  AERN-SV-DISFRMLA     PIC X.
           03  AERN-SV-INTDATE      PIC 9(8)          COMP.
           03  AERN-SV-LOANDATE     PIC 9(8)          COMP.
           03  AERN-SV-1STPYDATE    PIC 9(8)          COMP.
           03  AERN-SV-ORGTERM   PIC 999              COMP.
           03  AERN-SV-ENDDATE   PIC S9(8)            COMP.
           03  AERN-SV-1STPYAMT  PIC 9(6)V99          COMP.
           03  AERN-SV-REGPYAMT  PIC 9(6)V99          COMP.
           03  AERN-SV-APRATE    PIC 999V999          COMP.
           03  AERN-SV-ANTICTERM PIC 999              COMP.
           03  AERN-SV-LASTPYAMT PIC 9(6)V99          COMP.
           03  AERN-SV-NOREGPY   PIC 999              COMP.

           03  AERN-PAYMENTS     PIC 9(5)V9(8).
           03  AERN-PAYMENTSX   REDEFINES AERN-PAYMENTS.
               05  AERN-WPAYS    PIC 9(5).
               05  AERN-PPAYS    PIC V9(8).
           03  AERN-PAYMNTD      PIC S9(7)V99         COMP.
           03  AERN-WORKER       PIC S9(6)V9(12)      COMP.

      * ADDON CONSOLIDATION FIELDS:
           03  AERN-LN-ORIG-1STPYDATE  PIC 9(8)       COMP.
           03  AERN-LN-PURDATE         PIC 9(8)       COMP.
           03  AERN-LN-LOANDATE        PIC 9(8)       COMP.
           03  AERN-LN-INTDATE         PIC 9(8)       COMP.
           03  AERN-LN-TOTNODEF      PIC S999         COMP.
           03  AERN-LN-ANTICTERM     PIC S999         COMP.
           03  AERN-LN-SALEFIN       PIC X.
           03  AERN-LN-ANTICERN      PIC S9(6)V99     COMP.
           03  AERN-LN-ANTICADJ      PIC S9(6)V99     COMP.
           03  AERN-LN-ACCERN        PIC S9(6)V99     COMP.
           03  AERN-LN-UFERN         PIC S9(6)V99     COMP.
           03  AERN-LN-REBATE        PIC X.
           03  AERN-LN-APRATE        PIC S9(3)V9(4)    COMP.
           03  AERN-LN-LNAMT         PIC S9(7)V99      COMP.

      * WORK LOAN FIELDS:
           03  AERN-WK-LOANDATE         PIC 9(8)         COMP.
           03  AERN-WK-INTDATE          PIC 9(8)         COMP.
           03  AERN-WK-PURDATE          PIC 9(8)         COMP.
           03  AERN-WK-ORIG-1STPYDATE   PIC 9(8)         COMP.
           03  AERN-WK-1STPYDATE        PIC 9(8)         COMP.
           03  AERN-WK-ORGTERM          PIC 9(3)         COMP.
           03  AERN-WK-SUM-ORGTERM      PIC 9(3)         COMP.
           03  AERN-WK-1STPYAMT         PIC 9(7)V99      COMP.
           03  AERN-WK-NOREGPY          PIC 9(3)         COMP.
           03  AERN-WK-REGPYAMT         PIC 9(7)V99      COMP.
           03  AERN-WK-LASTPYAMT        PIC 9(7)V99      COMP.
           03  AERN-WK-ORIGPURAMT       PIC 9(7)V99      COMP.
           03  AERN-WK-ANTICERN         PIC S9(6)V99     COMP.
           03  AERN-WK-ANTICADJ         PIC S9(6)V99     COMP.
           03  AERN-WK-ACAERN           PIC S9(6)V99     COMP.
           03  AERN-WK-UFERN            PIC S9(6)V99     COMP.
           03  AERN-WK-FINCHG           PIC 9(7)V99      COMP.
           03  AERN-WK-ANTICTERM        PIC S999         COMP.
           03  AERN-WK-TOTNODEF         PIC S999         COMP.
           03  AERN-WK-APRATE           PIC 999V999      COMP.
           03  AERN-WK-FINANCED         PIC 9(7)V99      COMP.
           03  AERN-WK-REBATE           PIC X.
           03  AERN-WK-DISFRMLA         PIC X.
           03  AERN-WK-INCLUDE-DEF-FG   PIC X.

      *------------------------------------------------------------
           03  AERN-MDTE-FLAG           PIC X.
           03  AERN-MDTE-ORGTERM-FG     PIC X.
           03  AERN-WORKNO              PIC S9(5)          COMP.
           03  AERN-HOLD                PIC 9              VALUE 0.
           03  AERN-HOLD-ANTICTERM      PIC S999           COMP.
           03  AERN-HOLD-ANTICERN       PIC S9(6)V99       COMP.
           03  AERN-HOLD-ANTICADJ       PIC S9(6)V99       COMP.
           03  AERN-HOLD-ACCERN         PIC S9(6)V99       COMP.
           03  AERN-HOLD-UFERN          PIC S9(6)V99       COMP.
           03  AERN-PROFILE-WORKER      PIC 9(12).
           03  FILLER                  REDEFINES AERN-PROFILE-WORKER.
               05  AERN-PROFILE-WK      OCCURS 12
                                        PIC 9.
      *----RE-ADJUST ACCRUAL EARNINGS TO CASH EARNINGS VIA ACCRUAL
      *    NEGATIVE EARNINGS
           03  AERN-CERN-TOTLCHG        PIC 9(6)V99 COMP.
           03  AERN-CERN-SAVE-PAYMNTD   PIC 9(7)V99 COMP.
           03  AERN-CERN-PAYMNTD        PIC 9(7)V99 COMP.
           03  AERN-CERN-SAVE-WKDATE1   PIC 9(8) COMP.
           03  AERN-CERN-WKDATE1        PIC 9(8) COMP.
           03  AERN-CERN-PAYMENTS       PIC 9(5)V9(8).
           03  AERN-CERN-PAYMENTSX      REDEFINES AERN-CERN-PAYMENTS.
               05  AERN-CERN-WPAYS         PIC 9(5).
               05  AERN-CERN-PPAYS         PIC V9(8).
           03  AERN-CERN-SAVE-WPAYS        PIC 9(5) COMP.

