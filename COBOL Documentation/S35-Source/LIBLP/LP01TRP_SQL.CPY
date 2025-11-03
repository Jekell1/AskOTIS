      * COPYMEMBER: LIBLP/LP01TRP_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY TRANSACTION FILE - PAYMENTS
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      * BAH 20220923 ADDED TRP-LCDUE #1570
      * BAH 20240212 ADDED TRP-REPAY-TRANS-ID #1651
      * BAH 20250115 ADDED TRP-CLOSFEE-REB-AMT #1697 S35Q-157
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRP1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRP1-CURSOR-STAT-GOOD             VALUE "00".
           88  TRP1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRP2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRP2-CURSOR-STAT-GOOD             VALUE "00".
           88  TRP2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  TRP1-RETURN-CODE                PIC S9(4)   COMP-5.
       01  TRP2-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QTRP1-WHERE-BEG-KEY.
           03  QTRP1-WBEG-BRNO             PIC 9(04).
           03  QTRP1-WBEG-DATE             PIC X(10).
           03  QTRP1-WBEG-CLASS            PIC 9(03).
           03  QTRP1-WBEG-ACCTNO           PIC 9(08).
           03  QTRP1-WBEG-LINE             PIC 9(03).

       01  QTRP1-WHERE-END-KEY.
           03  QTRP1-WEND-BRNO             PIC 9(04).
           03  QTRP1-WEND-DATE             PIC X(10).
           03  QTRP1-WEND-CLASS            PIC 9(03).
           03  QTRP1-WEND-ACCTNO           PIC 9(08).
           03  QTRP1-WEND-LINE             PIC 9(03).

       01  QTRP2-WHERE-BEG-KEY.
           03  QTRP2-WBEG-DATE             PIC X(10).
           03  QTRP2-WBEG-BRNO             PIC 9(04).
           03  QTRP2-WBEG-CLASS            PIC 9(03).
           03  QTRP2-WBEG-ACCTNO           PIC 9(08).
           03  QTRP2-WBEG-LINE             PIC 9(03).

       01  QTRP2-WHERE-END-KEY.
           03  QTRP2-WEND-DATE             PIC X(10).
           03  QTRP2-WEND-BRNO             PIC 9(04).
           03  QTRP2-WEND-CLASS            PIC 9(03).
           03  QTRP2-WEND-ACCTNO           PIC 9(08).
           03  QTRP2-WEND-LINE             PIC 9(03).

       01  QTRP1-WHERE-SCAN-BEG-KEY.
           03  QTRP1-WSBEG-DATE            PIC X(10).
           03  QTRP1-WSBEG-CLASS           PIC 9(03).
           03  QTRP1-WSBEG-ACCTNO          PIC 9(08).
           03  QTRP1-WSBEG-LINE            PIC 9(03).
 
       01  QTRP-REC.
           03  QTP-BRNO             PIC 9(4).
           03  QTP-DATE             PIC X(10).
           03  QTP-CLASS            PIC 999.
           03  QTP-ACCTNO           PIC 9(8).
           03  QTP-LINE             PIC 999.
           03  QTP-LTOUCH-DATE      PIC X(10).
           03  QTP-LAWCODE          PIC 99.
           03  QTP-JDCD             PIC X.
           03  QTP-PLCD             PIC X.
           03  QTP-SSNO             PIC 9(10).
           03  QTP-MAKER            PIC X.
           03  QTP-IBPC             PIC X.
           03  QTP-TRCD             PIC XX.
           03  QTP-REFNO            PIC X(5).
           03  QTP-PAYDATE          PIC X(10).
           03  QTP-DUEDATE          PIC X(10).
           03  QTP-TRAMT            PIC S9(7)V99.
           03  QTP-INTDUE           PIC S9(7)V99.
           03  QTP-APOTH            PIC S9(7)V99.
           03  QTP-APOT2            PIC S9(7)V99.
           03  QTP-APINT            PIC S9(7)V99.
           03  QTP-APLC             PIC S9(7)V99.
           03  QTP-APCUR            PIC S9(7)V99.
           03  QTP-GLNO-1         PIC 9(11).
           03  QTP-GLAMT-1        PIC S9(7)V99.
           03  QTP-GLNO-2         PIC 9(11).
           03  QTP-GLAMT-2        PIC S9(7)V99.
           03  QTP-GLNO-3         PIC 9(11).
           03  QTP-GLAMT-3        PIC S9(7)V99.
           03  QTP-EARNINGS-1     PIC S9(7)V99.
           03  QTP-EARNINGS-2     PIC S9(7)V99.
           03  QTP-EARNINGS-3     PIC S9(7)V99.
           03  QTP-EARNINGS-4     PIC S9(7)V99.
           03  QTP-EARNINGS-5     PIC S9(7)V99.
           03  QTP-EARNINGS-6     PIC S9(7)V99.
           03  QTP-EARNINGS-7     PIC S9(7)V99.
           03  QTP-EARNINGS-8     PIC S9(7)V99.
           03  QTP-EARNINGS-9     PIC S9(7)V99.
           03  QTP-EARNINGS-10    PIC S9(7)V99.
           03  QTP-WORKAMT-1     PIC S9(7)V99.
           03  QTP-WORKAMT-2     PIC S9(7)V99.
           03  QTP-WORKAMT-3     PIC S9(7)V99.
           03  QTP-WORKAMT-4     PIC S9(7)V99.
           03  QTP-WORKAMT-5     PIC S9(7)V99.
           03  QTP-WORKAMT-6     PIC S9(7)V99.
           03  QTP-WORKAMT-7     PIC S9(7)V99.
           03  QTP-WORKAMT-8     PIC S9(7)V99.
           03  QTP-WORKAMT-9     PIC S9(7)V99.
           03  QTP-WORKAMT-10    PIC S9(7)V99.
           03  QTP-OTHBAL           PIC S9(7)V99.
           03  QTP-OT2BAL           PIC S9(7)V99.
           03  QTP-INTBAL           PIC S9(7)V99.
           03  QTP-LCBAL            PIC S9(7)V99.
           03  QTP-CURBAL           PIC S9(7)V99.
           03  QTP-PDTH-DATE        PIC X(10).
           03  QTP-DLNO             PIC 9(4)V99.
           03  QTP-DLPROC           PIC S9(7)V99.
           03  QTP-SERVACCT         PIC X.
           03  QTP-SALEFIN          PIC X.
           03  QTP-INSURED          PIC X.
           03  QTP-INSOURS          PIC X.
           03  QTP-LOANTYPE         PIC X.
           03  QTP-TILLNO           PIC 9(2).
           03  QTP-LCPDTH-DATE      PIC X(10).
           03  QTP-POSTING-BR       PIC 9(4).
           03  QTP-OTHBR-FG         PIC X.
           03  QTP-REV              PIC X.
           03  QTP-REVTRCD          PIC XX.
           03  QTP-REVSEQNO         PIC 9(5).
           03  QTP-STATUSFG         PIC X.
           03  QTP-KEEPAPINT        PIC X.
           03  QTP-INTPAID          PIC S9(7)V99.
           03  QTP-PRIOR-POOLID     PIC 9(5).
           03  QTP-INSTYPES         PIC X.
           03  QTP-INSEARN-1        PIC S9(7)V99.
           03  QTP-INSEARN-2        PIC S9(7)V99.
           03  QTP-INSEARN-3        PIC S9(7)V99.
           03  QTP-INSEARN-4        PIC S9(7)V99.
           03  QTP-INSEARN-5        PIC S9(7)V99.
           03  QTP-INSEARN-6        PIC S9(7)V99.
           03  QTP-INSEARN-7        PIC S9(7)V99.
           03  QTP-INSEARN-8        PIC S9(7)V99.
           03  QTP-INSEARN-9        PIC S9(7)V99.
           03  QTP-INSEARN-10       PIC S9(7)V99.
           03  QTP-INSWORKAMT-1        PIC S9(7)V99.
           03  QTP-INSWORKAMT-2        PIC S9(7)V99.
           03  QTP-INSWORKAMT-3        PIC S9(7)V99.
           03  QTP-INSWORKAMT-4        PIC S9(7)V99.
           03  QTP-INSWORKAMT-5        PIC S9(7)V99.
           03  QTP-INSWORKAMT-6        PIC S9(7)V99.
           03  QTP-INSWORKAMT-7        PIC S9(7)V99.
           03  QTP-INSWORKAMT-8        PIC S9(7)V99.
           03  QTP-INSWORKAMT-9        PIC S9(7)V99.
           03  QTP-INSWORKAMT-10       PIC S9(7)V99.
           03  QTP-DLRVCHGS-ADJ-1      PIC S9(7)V99.
           03  QTP-DLRVCHGS-ADJ-2      PIC S9(7)V99.
           03  QTP-DLRVCHGS-ADJ-3      PIC S9(7)V99.
           03  QTP-DLPARVHELD-ADJ       PIC S9(7)V99.
           03  QTP-DLPARVPAID-ADJ       PIC S9(7)V99.
           03  QTP-NEWSTAT-RATE         PIC 999V999.
           03  QTP-POCD                 PIC XX.
           03  QTP-PODATE               PIC X(10).
           03  QTP-DL-RVCHGS-CODE-1     PIC X.
           03  QTP-DL-RVCHGS-CODE-2     PIC X.
           03  QTP-DL-RVCHGS-CODE-3     PIC X.
           03  QTP-DL-PARVHELD-CODE     PIC X.
           03  QTP-DL-PARVPAID-CODE     PIC X.
           03  QTP-TOTNODEF             PIC S999.
           03  QTP-LNAMT                PIC S9(7)V99.
           03  QTP-ORIG-1STPYDATE       PIC X(10).
           03  QTP-OLD1ST-PYDATE        PIC X(10).
           03  QTP-1STPYDATE            PIC X(10).
           03  QTP-ORGTERM              PIC 9(3).
           03  QTP-INSCOMM              PIC S9(5)V99.
           03  QTP-MANUAL-RB            PIC X.
           03  QTP-USERID               PIC X(10).
           03  QTP-POSTTIME             PIC 9(6).
           03  QTP-PLTYPE               PIC X.
           03  QTP-PL-OTH-AR            PIC S9(7)V99.
           03  QTP-LP-DLNO              PIC 9(4)V99.
           03  QTP-REPOCD               PIC X.
           03  QTP-SPRCLASS             PIC 999.
           03  QTP-MAX-DEFER-FG         PIC X.
           03  QTP-POSTDATE             PIC X(10).
           03  QTP-UNITPER-CD           PIC X.
           03  QTP-UNITPER-FREQ         PIC 9(3).
           03  QTP-SP-DEFFRMLA          PIC 99.
           03  QTP-COLLECTOR            PIC X(3).
           03  QTP-PENALTY-INT          PIC S9(7)V99.
           03  QTP-INSCOMP              PIC 99.
           03  QTP-PROMDATE             PIC X(10).
           03  QTP-PROMAMT              PIC S9(7)V99.
           03  QTP-PROMID               PIC X(3).
           03  QTP-INS-TAX-RB           PIC S9(7)V99.
           03  QTP-PROMDAYS-PASTDUE     PIC S9(3).
           03  QTP-MIP-FG               PIC X.
           03  QTP-MIP-CLPAID           PIC S9(7)V99.
           03  QTP-MIP-CLCOMM           PIC S9(7)V99.
           03  QTP-MIP-AHPAID           PIC S9(7)V99.
           03  QTP-MIP-AHCOMM           PIC S9(7)V99.
           03  QTP-MIP-CLPDTH-DATE      PIC X(10).
           03  QTP-MIP-AHPDTH-DATE      PIC X(10).
           03  QTP-INS-TAX-COUNTY-CD    PIC X(3).
           03  QTP-INS-TAX-CITY-CD      PIC X(3).
           03  QTP-APPLIED-TO-ESCROW    PIC S9(7)V99.
           03  QTP-ESCROW-AMT           PIC S9(7)V99.
           03  QTP-ESCROW-PREPAID       PIC S9(7)V99.
           03  QTP-ESCROW-DISBURSED     PIC S9(7)V99.
           03  QTP-LP-SEQNO             PIC 9(5).
           03  QTP-PRIOR-DF-CODE        PIC X(2).
           03  QTP-PRIOR-DF-POSTDATE    PIC X(10).
           03  QTP-CANCEL-REASON        PIC X(3).
           03  QTP-FROMCL               PIC 9(3).
           03  QTP-TOCL                 PIC 9(3).
           03  QTP-ADDON-TXT            PIC X(3).
           03  QTP-ADDON-TYP            PIC X(2).
           03  QTP-DLRVNUM              PIC 9.
           03  QTP-P4P5-TRANCD          PIC XX.
           03  QTP-P4P5-DLRVNUM         PIC 9.
           03  QTP-POOLID               PIC 9(5).
           03  QTP-ACTIONCD2            PIC X(4).
           03  QTP-LCDUE                PIC S9(7)V99.
           03  QTP-REPAY-TRANS-ID       PIC 9(10).
           03  QTP-CLOSFEE-REB-AMT      PIC S9(7)V99.

       EXEC SQL END DECLARE SECTION END-EXEC.
