      * COPYMEMBER: LIBCL/NCSCHGW
      ****************************************************
      *    NCSCHGW
      *    WORKERS FOR NORTH CAROLINA SERVICE CHARGE FORMULA 23:
      * REV:
      *  040422 BAH ADDED NCSCHG-LN-SSNO-2 THRU 4 AND FILLER, WAGNER PR# 2274
      *  131213 BAH ADDED NCSCHG-ORGST AND NCSCHG-CLASS, WORLD PR# 953
      *  2015/12/08 BAH EXPANDED DATE TO 9(8) AND CLASS TO 9(3), PD0003
      ****************************************************
       01  NCSCHG-WORKERS.
           03  NCSCHG-LN-ACCTNO           PIC 9(8).
           03  NCSCHG-LN-SSNO             PIC 9(9).
           03  NCSCHG-TRANS-DATE          PIC 9(8).
           03  NCSCHG-NO-SERCHG           PIC 9(2).
           03  NCSCHG-SCFRMLA             PIC 9(2).

           03  NCSCHG-RESTRICTED          PIC X.
           03  NCSCHG-LN-SSNO-2           PIC 9(9).
           03  NCSCHG-LN-SSNO-3           PIC 9(9).
           03  NCSCHG-LN-SSNO-4           PIC 9(9).

           03  NCSCHG-ORGST               PIC XX.
           03  NCSCHG-CLASS               PIC 9(3).
