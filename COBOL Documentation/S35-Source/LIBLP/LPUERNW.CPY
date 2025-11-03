      * COPYMEMBER: LIBLP/LPUERNW
      **************************************************************************
      *               (LIBLP/LPUERNW)
      *    UP FRONT EARNINGS CALCULATION WORKERS
      *
      * REV:
      *  JTG 122288 ADDED TABLE FOR 6 COMPONETS
      *  JTG 051089 CHANGED WORKERS TO COMP
      *  JTG 091598 ADDED NEW WORKER2 FOR SP-ERNPERCD = 'S', CITIZENS #733
      **************************************************************************
       01  UERNINGS-WORKERS.
      * INPUT FIELDS:
           03  UERN                PIC 9          COMP VALUE 0.

      * OUTPUT FIELDS:
           03  UERN-TOT-EARNINGS   PIC S9(7)V99   COMP.
           03  UERN-EARNINGS       OCCURS 6 TIMES
                                   PIC S9(7)V99   COMP.

      * WORK FIELDS:
           03  UERN-WORKER         PIC S9(7)V99   COMP.
           03  UERN-WORKER2        PIC S9(7)V99   COMP.
