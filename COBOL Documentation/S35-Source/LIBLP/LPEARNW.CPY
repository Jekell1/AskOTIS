      * COPYMEMBER: LIBLP/LPEARNW
      ****************************************************
      *    CALCULATION OF ANTICIPATED EARNINGS WORKERS
      *
      * REV:
      *  JTG 051089  CHANGED WORKERS TO COMP
      *  JTG 030790  ADDED EARN-ANTICADJ EARN-UFERN
      ****************************************************
       01  EARNINGS-WORKERS.
      * INPUT FIELDS:

      * OUTPUT FIELDS:
           03  EARN-TOT-ANTICERN   PIC S9(7)V99   COMP.
           03  EARN-TOT-ANTICADJ   PIC S9(7)V99   COMP.
           03  EARN-TOT-ACCERN     PIC S9(7)V99   COMP.
           03  EARN-TOT-UFERN      PIC S9(7)V99   COMP.
           03  EARN-TOT-UNEARN     PIC S9(7)V99   COMP.
           03  EARN-UNEARN         OCCURS 6 TIMES
                                   PIC S9(7)V99   COMP.
      * WORK FIELDS:
           03  EARN                PIC 9          COMP VALUE 0.
