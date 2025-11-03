      * COPYMEMBER: LIBCL/TXSCHGW
      ****************************************************
      *    TXSCHGW
      *    WORKERS FOR CL/TXSCHG.CBL
      *
      * REV:
      *  160511 BAH NEW FOR WORLD SCFRMLA 58, PR#1154
      *  181011 BAH ADDED TXSCHG-CLASS, #1341
      ****************************************************
       01  TXSCHG-WORKERS.
           03  TXSCHG-SCFRMLA             PIC 99.
           03  TXSCHG-PRIOR-ACCTNO        PIC 9(8).
           03  TXSCHG-LOANDATE            PIC 9(8).
           03  TXSCHG-SSNO                PIC 9(9).
           03  TXSCHG-ORGST               PIC XX.
           03  TXSCHG-CLASS               PIC 9(3).
           03  TXSCHG-RESTRICTED          PIC X.

