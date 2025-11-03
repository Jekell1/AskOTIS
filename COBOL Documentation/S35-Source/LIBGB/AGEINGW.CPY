      * COPYMEMBER: LIBGB/AGEINGW
      **************************************************************************
      *    AGEING          AGEING CALCULATION WORKERS
      * REV:
      *  JTG 051189 CHANGED WORKERS TO COMP
      *  JTG 040490 ADDED RDSUB, RECDEL, CSUB-ALDEL-FG
      *             AGEING-HOLD-SUB & AGEING-HOLD-AGEING
      *  JTG 062194 ADDED RDPOT30, CPOT30
      *  SLC 051895 ADDED AGEING-TOTPAYMNTD-SV
      *  JTG 072295 REDEFINED AGEING-DATE RE: MO/DA/YR
      *             ADDED CPOTCHGOFF & RDPOTCHGOFF
      *  MJD 071596 ADDED AGEING-HOLD-REMAIN FOR SP-BANKUNITPER-CD = "A"
      *  JTG 082897 ADDED AGEING-PERCENT-FACTOR FOR SP-BANKUNITPER-CD = "D"
      *  JTG 092297 ADDED AGEING-HOLD-REMAIN2 FOR SP-BANKUNITPER-CD = 'D'
      *  JTG 092497 ADDED AGEING-SKIP-CON-REC-FG WORLD A90
      *  JTG 022498 ADDED AGEING-NO-TOBE-PAID, AGEING-NO-PAID, AGEING-WK-DATE
      *             CHANGED SP-BANKUNITPER-CD = 'A' & 'D', INSTANT AUTO
      *             TO DETERMINE EXACT PAYMENT REQUIREMENT FOR A GIVEN
      *             MONTH. EX. WEEKLY, JUL 5 PAYMENTS, AUG 4 PAYMENTS
      *  JTG 990311 ADDED OUTPUT OF TFC 'CTFC-NM' TABLE INDEX
      *  MJD 110525 ADDED AGEING-SUB-WORKER/REMAIN FOR SP-BANKRULE = "F" #752
      *  BLM 170309 CHANGED AGEING-DATE TO BE YYYYMMDD, PD0006
      **************************************************************************
       01  AGEING-WORKERS.
      * YYYYMMDD
           03  AGEING-DATE            PIC 9(8).
      * FIELDS FOR COMPUTE-RECDEL:
           03  RDSUB                  PIC S9(6)      COMP.
           03  RECDEL                 PIC S9(5)      COMP.
           03  RDPOT30                PIC X          VALUE " ".
           03  RDPOTCHGOFF            PIC X          VALUE " ".
      * FIELDS FOR COMPUTE RECENCY:
           03  RSUB                   PIC S9(6)      COMP.
           03  RECENCY                PIC S9(5)      COMP.
      * FIELDS FOR COMPUTE-CONTRACTUAL:
           03  CSUB                   PIC S9(6)      COMP.
           03  CONTRACTUAL            PIC S9(5)      COMP.
           03  CPOT30                 PIC X          VALUE " ".
           03  CPOTCHGOFF             PIC X          VALUE " ".
           03  CSUB-ALDEL-FG          PIC X          VALUE " ".
           03  CTFC-NM                PIC S9(6)      COMP.
      * FIELDS FOR COMPUTE-PAID-AHEAD:
           03  PSUB                   PIC S9(6)      COMP.
      * AGEING WORK FIELDS:
           03  AGEING-HOLD            PIC S9(5)      COMP.
           03  AGEING-WORK            PIC S9(7)V9(9) COMP.
           03  AGEING-TOTPAYMNTD-SV   PIC S9(7)V99   COMP.
           03  AGEING-HOLD-REMAIN     PIC V9(12)     COMP.
           03  AGEING-PERCENT-FACTOR  PIC 9(3)V9(3)  COMP.
           03  AGEING-HOLD-CONTPDTH   PIC 9(4)       COMP.
           03  AGEING-NO-TOBE-PAID    PIC S9(4)      COMP.
           03  AGEING-NO-PAID         PIC S9(4)      COMP.
           03  AGEING-WK-DATE         PIC 9(8)       COMP.
           03  AGEING-SUB-WORKER      PIC S9(6)     COMP.
           03  AGEING-SUB-REMAIN      PIC 9(6)      COMP.
       01  AGEING-E-1STPYAMT     PIC S9(7)V99 COMP.
       01  AGEING-E-REGPYAMT     PIC S9(7)V99 COMP.
       01  AGEING-E-LASTPYAMT    PIC S9(7)V99 COMP.
