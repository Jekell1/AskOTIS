      * COPYMEMBER: LIBCL/SCSCHGW
      **********************************************************
      *    SCSCHGW
      *    WORKERS FOR SOUTH CAROLINA SERVICE CHARGE FORMULA:
      * REV:
      *  010518 BAH ADDED SCSCHG-SCFRMLA FOR SCFRMLA 42 (SUNBELT) PR# 1484
      *  170323 BAH CHANGED SCSCHG-TRANS-DATE FROM 9(6) TO 9(8)
      **********************************************************
       01 SCSCHG-WORKERS.
           03 SCSCHG-UPDATE-FG              PIC X.
           03 SCSCHG-ORGST                  PIC XX.
           03 SCSCHG-LN-SSNO                OCCURS 4 TIMES
                                            PIC 9(9).
           03 SCSCHG-PRIOR-ACCTNO           PIC 9(8).
           03 SCSCHG-NEW-ACCTNO             PIC 9(8).
           03 SCSCHG-TRANS-DATE             PIC 9(8).
           03 SCSCHG-PRIOR-CASHADV          PIC S9(7)V99.
           03 SCSCHG-RESTRICT-SERCHG        PIC S9(7)V99.
           03 SCSCHG-RESTRICT-HIGH-CASHADV  PIC S9(7)V99.
           03 SCSCHG-SCFRMLA                PIC 99.
