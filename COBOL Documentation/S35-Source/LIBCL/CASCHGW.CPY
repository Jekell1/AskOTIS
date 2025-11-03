      * COPYMEMBER: LIBCL/CASCHGW
      ****************************************************
      *    CASCHGW
      *    WORKERS FOR CL/CASCH16.C
      *
      * REV:
      *  2011.0127 BAH RENAMED CASCHG-ORGST TO CASCHG-SCFRMLA FOR
      *                SCFRMLA 49, REGMGM PR# 3721
      *  2017.0407 BAH CHANGED CASCHG-LOANDATE TO 9(8) FROM 9(6)
      *  2024.0401 BAH REMOVED CASCHG-YEARTYPE, CASCHG-ERRCD AND FILLER
      ****************************************************
       01  CASCHG-WORKERS.
           03  CASCHG-SCFRMLA             PIC 99.
           03  CASCHG-PRIOR-ACCTNO        PIC 9(8).
           03  CASCHG-LOANDATE            PIC 9(8).
           03  CASCHG-RESTRICTED          PIC X.
