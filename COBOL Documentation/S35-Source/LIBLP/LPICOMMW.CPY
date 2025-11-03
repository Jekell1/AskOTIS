      * COPYMEMBER: LIBLP/LPICOMMW
      *----------------------------------------------------------
      *                (LPICOMMW)
      *    INSURANCE COMMISSION WORKERS
      * REV:
      *  062095 JTG INCREASED ICOMM & ICOMM-SUB1 FOR UP TO 10
      *----------------------------------------------------------
       01  ICOMM-WORKERS.
      * INPUT FIELDS:
           03  ICOMM                 PIC 99       COMP VALUE 0.
           03  ICOMM-INSCOMP         PIC 99       COMP VALUE 0.
           03  ICOMM-INSPREM         PIC 9(6)V99  COMP VALUE 0.
           03  ICOMM-INSOURS         PIC X             VALUE "N".

      * OUTPUT FIELDS:
           03  ICOMM-INSCOMM         PIC 9(6)V99  COMP VALUE 0.

      * WORKERS:
           03  ICOMM-SUB1            PIC 99       COMP.
           03  ICOMM-IC-CODE         PIC 99       COMP VALUE 0.
