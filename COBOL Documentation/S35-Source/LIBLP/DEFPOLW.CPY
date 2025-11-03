      * COPYMEMBER: LIBLP/DEFPOLW
      *--------------------------------------------------------
      *          (LIBLP/DEFPOLW)
      *    DEFERMENT POLICY TEST WORKERS
      * REV:
      *--------------------------------------------------------
      * INPUT:
       01  DEFPOL-TRCD         PIC XX.
       01  DEFPOL-DATE         PIC 9(8) COMP.
      * OUTPUT:
       01  DEFPOL-MAXIMUM      PIC X.
      * WORKERS:
       01  DEFPOL-WORKER.
           03  FILLER          PIC X.
           03  DEFPOL-FTST     PIC X.
           03  DEFPOL-NODF    REDEFINES DEFPOL-FTST
                               PIC 9.
           03  DEFPOL-LP-REC   PIC X(LP-SIZE).
