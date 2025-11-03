      * COPYMEMBER: LIBWI/BILNSC.CMD
       01  LEGEND-01                  PIC X(40)  VALUE
           "Scan Type: 1-Borr ssno, 2-Borrower name".
       01  LEGEND-02                  PIC X(40)  VALUE
           "F7-Exit, ^-Type, Enter Soc-Sec-No:".
       01  LEGEND-03                  PIC X(40)  VALUE
           "F7-Exit, ^-Type, Enter Lastname,First:".
       01  LEGEND-04.
           03  FILLER                 PIC X(40)  VALUE
           "F7, F1, F3-Open, F4-All, F6-More Info, S".
            03  FILLER                PIC X(20)  VALUE
           "elect Borrower(rtn)".
       01  LEGEND-04G.
           03  FILLER                 PIC X(40)  VALUE
           "F7, F1, F3-Open, F4-All, F6-More Info:".
       01  LEGEND-05                  PIC X(5)   VALUE
           "Scan:".
       01  LEGEND-06                  PIC X(5)   VALUE
           "Type:".
      * for interbranch payments:
       01  LEGEND-07.
           03  FILLER                 PIC X(40)  VALUE
           "F7, F3-Open, F4-All, F6-More Info, Selec".
           03  FILLER                 PIC X(20)  VALUE
           "t Borrower(rtn):".
      * for global loan scan:
       01  LEGEND-07G.
           03  FILLER                 PIC X(40)  VALUE
           "F7, F3-Open, F4-All, F6-More Info:".
       01  SSNO-PROMPT.
           03  FILLER                 PIC X(39)  VALUE 
           "Ssno         Name                  Acct".
           03  FILLER                 PIC X(37)  VALUE 
           "no     M  Brno  St  Loan Date Paidoff".
       01  NAME-PROMPT.
           03  FILLER                 PIC X(39)  VALUE
           " Name                Ssno          Acct".
           03  FILLER                 PIC X(37)  VALUE
           "no     M  Brno  St  Loan Date Paidoff".
       01  TITLE-01.
           03  FILLER                 PIC X(28)  VALUE
           " Global Borrower/Loan Scan  ".
       01  TITLE-02.
           03  FILLER                 PIC X(28)  VALUE
           "   Inter Branch Loan Scan   ".

