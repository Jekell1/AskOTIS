      * COPYMEMBER: LIBLP/LPDLADJW
      *--------------------------------------------------------------
      *                    (LPDLADJW)
      *    CALCULATE LOAN Z3 WORKERS
      *  REV:
      *--------------------------------------------------------------
       01  LPDLADJ-WORKERS.
      * INPUT FIELDS:

      * OUTPUT FIELDS:
           03  DLADJ-INSADJ-OURS            PIC S9(7)V99    COMP.

      * WORKERS:
           03  DLADJ                        PIC 99          COMP.
           03  DLADJ-FEE-AMT                PIC S9(7)V99    COMP.
           03  DLADJ-INS-OURS               PIC S9(7)V99    COMP.
