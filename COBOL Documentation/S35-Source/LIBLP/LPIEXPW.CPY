      * COPYMEMBER: LIBLP/LPIEXPW
      *****************************************************************
      *                    (LIBLP/LPIEXPW)
      *    COMPUTATION OF INS. EFF/EXP DATES AND COMMISSION WORKERS
      * REV:
      *****************************************************************
      * INPUT:
       01  IEXP-LN             PIC 9(2)     COMP.

      * OUTPUT:
       01  IEXP-EFFDATE        PIC 9(8)     COMP.
       01  IEXP-EXPDATE        PIC 9(8)     COMP.
       01  IEXP-INSCOMM        PIC 9(6)V99  COMP.

      * WORKERS:
       01  IEXP-IC             PIC 9(2)     COMP.
       01  IEXP-IC-CODE        PIC 99       COMP    VALUE 0.

