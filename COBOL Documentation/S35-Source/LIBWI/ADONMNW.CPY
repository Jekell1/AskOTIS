      * COPYMEMBER: LIBWI/ADONMNW
      * REV:
      *  20240412 BAH REMOVED ADONMN-TRANS-DATE, ADONMN-PASSWD
      *  2024.0503 BAH MADE LEVEL 01, CALL NOT WINDOW
       01  ADONMN-BUF.
           03  ADONMN-BRANCH         PIC 9(4).
           03  ADONMN-ACCTNO         PIC 9(8).
           03  ADONMN-PLCD           PIC X.
           03  ADONMN-SALEFIN        PIC X.
           03  ADONMN-SERVACCT       PIC X.
           03  ADONMN-LOANTYPE       PIC X.
           03  ADONMN-NEW-FLAG       PIC 9.
           03  ADONMN-TEST-BAL       PIC 9.
           03  ADONMN-LOANDATE       PIC 9(8) COMP-3.
           03  ADONMN-ORIG-1STPYDATE PIC 9(8) COMP-3.
