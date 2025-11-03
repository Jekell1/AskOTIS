      * COPYMEMBER: LIBLP/SETCTBLW

      ***************************************************************
      *                      SETCTBLW
      *          SET INDEX INTO SPR RATE TABLES WORKERS
      *
      * REV :
      ***************************************************************
      * INPUT FIELDS:
       01  CTBL-TERM                PIC S9(3)    COMP.
       01  CTBL-INT-CHARGEABLE      PIC S9(7)V99 COMP.

       01  CTBL-WORKER              PIC 9.
      * OUTPUT FIELDS:
       01  CTBL-INDEX               PIC 9.
