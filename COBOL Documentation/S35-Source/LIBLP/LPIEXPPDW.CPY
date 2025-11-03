      * COPYMEMBER: LIBLP/LPIEXPPDW
      *****************************************************************
      *                    (LIBLP/LPIEXPPDW)
      *    COMPUTATION OF INS. EFF/EXP DATES AND COMMISSION WORKERS
      * REV:
      *  MJD 011597 CREATED NEW VERSION FOR PD-REC.
      *****************************************************************
      * INPUT:
       01  IEXP-PD             PIC 9(2)     COMP.

      * OUTPUT:
       01  IEXP-EFFDATE        PIC 9(8)     COMP.
       01  IEXP-EXPDATE        PIC 9(8)     COMP.
       01  IEXP-INSCOMM        PIC 9(6)V99  COMP.

      * WORKERS:
       01  IEXP-IC             PIC 9(2)     COMP.
       01  IEXP-IC-CODE        PIC 99       COMP    VALUE 0.

