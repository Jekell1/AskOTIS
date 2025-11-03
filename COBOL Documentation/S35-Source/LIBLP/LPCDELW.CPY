      * COPYMEMBER: LIBLP/LPCDELW
      *-------------------------------------------------------------------
      *    CONTRACTUAL DELINQUENCY WORKERS
      *                (ARREARAGE)
      *
      * REV:
      *  JTG 051089 CHANGED WORKERS TO COMP
      *  JTG 080690 ADDED CDEL FOR SUBSCRIPTING
      *  JTG 070191 ADDED CDEL-WORKER
      *  JTG 012398 ADDED CDEL-LPCRNO-FG, RE: LPCRNO LDUE & NXDUE AMOUNTS
      *             ZERO RE: TDUE = 01/31/98 NXDUE = 02/28/98
      *-------------------------------------------------------------------
       01  CDEL-WORKERS.
           03  CDEL-DELINQUENT    PIC S9(7)V99 COMP.
           03  CDEL-ARREARAGE     PIC S9(7)V99 COMP.
           03  CDEL               PIC S999     COMP.
           03  CDEL-WORKER        PIC S9(7)V99 COMP.
           03  CDEL-LPCRNO-FG     PIC X        VALUE " ".
