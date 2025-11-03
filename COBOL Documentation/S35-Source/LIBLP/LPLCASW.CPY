      * COPYMEMBER: LIBLP/LPLCASW
      *----------------------------------------------------
      *    LATE-CHARGE ASSESS WORKERS
      *
      *  REV:
      *   JTG 051089 CHANGED WORKERS TO COMP.
      *   JTG 120590 ADDED LCAS-ELAPSED-DAYS FOR LCFRMLA A
      *   JTG 071696 ADDED LCAS-LCGRACE
      *   JTG 122398 ADDED LCAS-GRACE-DATE
      *   BAH 2022.0121 REMOVED REDEFINES OF LCAS-1STPYDATE & LCAS-LCPDTH-DATE
      *----------------------------------------------------
      * IN:
       01  LCAS-PAYDATE       PIC 9(8)  COMP.
       01  LCAS-1STPYDATE     PIC 9(8).
       01  LCAS-LCPDTH-DATE   PIC 9(8).

      * OUT:
       01  LCAS-ASSESS            PIC X.
       01  LCAS-ELAPSED-DAYS      PIC S9(5) COMP.
       01  LCAS-GRACE-DATE        PIC 9(8)  COMP.

      * WORKERS:
       01  LCAS-WORKERS.
           03  LCAS-WKDATE        PIC 9(8).
           03  FILLER            REDEFINES LCAS-WKDATE.
               05  FILLER         PIC 9(4).
               05  LCAS-WKMMDD    PIC 9(4).
           03  LCAS-HOLIDAY-FLAG  PIC 9   COMP VALUE 1.
           03  LCAS-SUB           PIC 99  COMP.
           03  LCAS-LCGRACE       PIC 999 COMP.
