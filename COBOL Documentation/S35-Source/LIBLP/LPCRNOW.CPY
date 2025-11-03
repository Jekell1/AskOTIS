      * COPYMEMBER: LIBLP/LPCRNOW
      *----------------------------------------------------------------------
      *            LPCRNOW
      *    CREATE NOTICE RECORD WORKERS
      *
      * REV:
      *  JTG 010290 ADDED CRNO-LPREC-FG
      *  JTG 052290 ADED CRNO-AVCR-RESTRICT-FG (GA)
      *  JTG 022593 ADDED CRNO-LPTRCD
      *  JTG 120194 ADDED CRNO-RECAST-FG (WISCONSIN ACCEPTANCE)
      *  JTG 102896 ADDED CRNO-GET-PAYOFF-ONLY, FOR PAYOFF GOOD THRU ROUTINE
      *  BAH 010305 ADDED CRNO-HOLD-CONTRFRMLA AND CRNO-HOLD-TOTPAYMNTD 
      *             WORLD, PR# 254, FOR GP-IGNORE-LCHG-NXDUE
      *  JTG 020123 EXPANDED PAYMNTD FIELDS TO 1,000,000        LENDMARK PR#J021
      *----------------------------------------------------------------------
       01  CRNO-WORKERS.
           03  CRNO-PAYDATE          PIC 9(8)   COMP.
           03  CRNO-MAKERCD          PIC X.
           03  CRNO-HOLDDA           PIC 99     COMP.
           03  CRNO-LPTRCD           PIC XX     VALUE "PY".
           03  CRNO-LPREC-FG         PIC X      VALUE " ".
           03  CRNO-AVCR-RESTRICT-FG PIC X      VALUE " ".
           03  CRNO-DEF-STOP         PIC X      VALUE " ".
           03  CRNO-RECAST-FG        PIC X      VALUE " ".
           03  CRNO-GET-PAYOFF-ONLY  PIC X      VALUE " ".
           03  CRNO-HOLD-CONTRFRMLA  PIC X      VALUE " ".
           03  CRNO-HOLD-TOTPAYMNTD  PIC S9(7)V99 COMP-3 VALUE 0.

