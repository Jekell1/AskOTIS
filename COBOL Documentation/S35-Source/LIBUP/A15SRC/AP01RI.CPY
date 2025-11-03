      * COPYMEMBER: LIBAP/AP01RI
      **************************************************
      *    ACCOUNTS PAYABLE RECURRING INVOICE FILE     *
      *    KEY1 = VENDOR NO. / INVOICE NUMBER          *
      **************************************************
       01  RI-REC.
           03  RI1-KEY.
               05  RI-VENDOR      PIC 9(6).
               05  RI-INVOICE     PIC X(10).
           03  RI-START-DATE      PIC 9(6)     COMP-3.
           03  RI-EXPIRE-DATE     PIC 9(6)     COMP-3.
           03  RI-EXPIRE-TIMES    PIC 999      COMP-3.
           03  RI-TIMES-REL       PIC 999      COMP-3.
           03  RI-LAST-DATE-REL   PIC 9(6)     COMP-3.
           03  RI-INTERVAL        PIC 999      COMP-3.
           03  RI-INTERVAL-TYPE   PIC X.
           03  RI-REFNO           PIC X(15).
           03  RI-AMOUNT          PIC S9(7)V99 COMP-3.
           03  RI-NONDISC         PIC S9(7)V99 COMP-3.
           03  RI-TERMS           PIC 99       COMP-3.
           03  RI-DISC            PIC S9(5)V99 COMP-3.
           03  RI-GLNO            PIC 9(11)    COMP-3 OCCURS 10.
           03  RI-GLAMT           PIC S9(7)V99 COMP-3 OCCURS 10.
           03  RI-PURORD          PIC X(12).
           03  RI-AUTH            PIC X(10).
           03  RI-LOC             PIC 9(4).
           03  FILLER             PIC X(25).
