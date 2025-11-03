      * COPYMEMBER: LIBLP/LPLCAPW
      *-------------------------------------------------------------------------
      *    LATE-CHARGE APPLY WORKER
      *  REV:
      *   JTG 051089 CHANGED WORKERS TO COMP
      *   JTG 051889 ADDED LCAP-TOTPAYMNTD FOR GENERIC CONTRACTUAL
      *   JTG 062190 ADDED LCAP-PRIOR-1STPY (PACESETTER F,H,I)
      *   JTG 072490 ADDED LCAP-APLC FOR LATE CHG'S APPLIED
      *   SLC 090690 ADDED LCAP, LCAP-WORKER, LCAP-CNT FOR ADDON.
      *   JTG 071596 EXPANDED LCAP-LCPDTH-DATE TO 9(6) TO INCLUDE LCAP-LCPDDD
      *   JTG 060898 ADDED LCAP-HOLDER AS A WORKER FOR LCAP-I-L
      *   JTG 000111 ADDED LCAP-NOLCHG  WORLD #218
      *              TO TRACK THE NUMBER OF LATE CHARGES ASSESSED
      *   JTG 000822 ADDED LOGIC FOR KENTUCKY 60 DAY NO CHARGE LAW, ADDED
      *              LCAP-POFF-LPTRCD, RE: SP-RBSPOPT1() = 19, WORLD #233
      *   JTG 010209 ADDED LCAP-TRAMT-PLUS-AP, IDEAL PR#315 LCAP-E
      *   JTG 010801 ADDED LCAP-APPLIED-TOWARD-SHORTAGE IDEAL PR#1509
      *   JTG 021115 ADDED LCAP-BATCH-REFCD LCAP-BATCH-REFCD-LC-FG
      *              ADDED NEW CODE FILE TYPE 'BR'-BATCH PAYMENT REFERENCE CODES
      *              BATCH PAYMENT REVISIONS                        REGACC #0006
      *   BLM 100330 ADDED LCAP-ESCROW-AMT, REGWAN PR# 3506
      *   BLM 170309 FIX LCAP-1STPYDATE,LCAP-LCPDTH-DATE AND LCAP-CURDATE
      *              FOR CCYYMMDD FORMAT, PD0006
      *   BAH 2024.0409 REMOVED LCAP-AMOUNT-LEFT-TO-APPLY,
      *                 LCAP-AMT-TO-ADVANCE-LCPDTH , LCAP-AMOUNT-PAID-BEFORE,
      *                 LCAP-DAYS-LATE, LCAP-NUMBER-OF-PMTS,
      *                 LCAP-MUST-APPLY-TO-ASSESS, LCAP-CHARGES-TOWARDS-PMTS,
      *                 LCAP-DO-WE-USE-PARTIALS
      *-------------------------------------------------------------------------
       01  LCAP-WORKERS.
      * INPUT FIELDS:
           03  LCAP-TRAMT                PIC S9(7)V99    COMP.
           03  LCAP-PAYDATE              PIC 9(8)        COMP VALUE 0.
           03  LCAP-1STPYDATE            PIC 9(8).
           03  LCAP-1STPYDATEX          REDEFINES LCAP-1STPYDATE.
               05  LCAP-1STPYDATE-CC     PIC 99.
               05  LCAP-1STPYDATE-YY     PIC 99.
               05  LCAP-1STPYDATE-MM     PIC 99.
               05  LCAP-1STPYDATE-DD     PIC 99.
           03  LCAP-LPTRCD               PIC XX          VALUE SPACES.
           03  LCAP-LPAPINT              PIC S9(6)V99    COMP VALUE 0.
           03  LCAP-LPAPLC               PIC S9(5)V99    COMP VALUE 0.
           03  LCAP-POFF-LPTRCD          PIC XX          VALUE SPACES.
           03  LCAP-BATCH-REFCD          PIC X(5)        VALUE SPACES.
           03  LCAP-BATCH-REFCD-LC-FG    PIC X           VALUE SPACES.
           03  LCAP-ESCROW-AMT           PIC S9(7)V99    COMP VALUE 0.

      * OUTPUT FIELDS:
           03  LCAP-OWE                  PIC S9(7)V99 VALUE 0 COMP.
           03  LCAP-APP                  PIC S9(7)V99 VALUE 0 COMP.
           03  LCAP-LCPDTH-DATE          PIC 9(8).
           03  LCAP-LCX                  REDEFINES LCAP-LCPDTH-DATE.
               05  LCAP-LCPDCC           PIC 99.
               05  LCAP-LCPDYY           PIC 99.
               05  LCAP-LCPDMM           PIC 99.
               05  LCAP-LCPDDD           PIC 99.
           03  LCAP-LCPAID               PIC S9(7)V99 VALUE 0 COMP.
           03  LCAP-PARTIALS             PIC S9(7)V99 VALUE 0 COMP.
           03  LCAP-NOLCHG               PIC S999     VALUE 0 COMP.

      * WORKERS:
           03  LCAP-HOLDER                 PIC S9(7)V99    COMP.
           03  LCAP-DELFAC                 PIC S9(7)V99    COMP.
           03  LCAP-WRK                    PIC S9(7)V99    COMP.
           03  LCAP-WRK2                   PIC S9(7)V99    COMP.
           03  LCAP-AMTNEEDED              PIC S9(7)V99    COMP.
           03  LCAP-TOTPAYMNTD             PIC S9(7)V99    COMP.
           03  LCAP-PAYMNTD                PIC S9(7)V99    COMP.
           03  LCAP-UNPAID-PORTION         PIC S9(7)V99    COMP.
           03  LCAP-PDTH-PAYMENTS          PIC 999         COMP.
           03  LCAP-TRAMT-PLUS-AP          PIC S9(7)V99    COMP.

           03  LCAP-LST-CURDATE            PIC 9(8)        COMP.
           03  LCAP-LST-CURDUE             PIC S9(7)V99    COMP.

           03  LCAP-CURDUE                 PIC S9(7)V99    COMP.
           03  LCAP-CURPMT                 PIC S9(7)V99    COMP.
           03  LCAP-CURDATE                PIC 9(8).
           03  LCAP-CURDATEX              REDEFINES LCAP-CURDATE.
               05  LCAP-CURDATE-CC         PIC 99.
               05  LCAP-CURDATE-YY         PIC 99.
               05  LCAP-CURDATE-MM         PIC 99.
               05  LCAP-CURDATE-DD         PIC 99.

           03  LCAP-PRIOR-1STPY            PIC X.

           03  LCAP-AL-OK                  PIC X.

           03  LCAP-WORKER                  PIC S9(7)V99 COMP.
           03  LCAP-CNT                     PIC S9(3)    COMP.
           03  LCAP                         PIC S999     COMP.

           03  LCAP-PAYMENTS                PIC 9999V9(12).
           03  LCAP-PY                     REDEFINES LCAP-PAYMENTS.
               05  LCAP-PAYS                PIC 9999.
               05  LCAP-REMAIN              PIC V9(12).
           03  LCAP-CUR-SCHD-PAY            PIC S9(7)V99 COMP.
           03  LCAP-PMT-NO                  PIC 999 COMP.
           03  LCAP-APPLIED-TOWARD-SHORTAGE PIC S9(7)V99 COMP.
