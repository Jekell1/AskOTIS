      * COPYMEMBER: LIBLP/LPITRMW
      *--------------------------------------------
      *    INSURANCE EXPIRATION TEST & TERM WORKERS
      * REV: JTG 051089 CHANGED FIELDS TO COMP
      *--------------------------------------------
       01  ITRM-WORKERS.
      * INPUT FIELDS:
           03  ITRM-SUB           PIC 9      COMP.
           03  ITRM-TODAY         PIC 9(8)   COMP VALUE 0.

      * OUTPUT FIELDS:
           03  ITRM-INSTERM       PIC 9(4)   COMP.
           03  ITRM-INEFFECT-FG   PIC X.

      * WORKERS:
           03  ITRM-SUB1          PIC 99     COMP.
