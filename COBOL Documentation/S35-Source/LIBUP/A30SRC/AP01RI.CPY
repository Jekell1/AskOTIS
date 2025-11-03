      * COPYMEMBER: LIBAP/AP01RI
      ******************************************************************
      *
      *    ACCOUNTS PAYABLE RECURRING INVOICE FILE     *
      *    KEY1 = VENDOR NO. / INVOICE NUMBER          *
      *
      *=================================================================
      * REV:
      * KEC 2015.1117 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED RI-SIZE
      *          INCREASE RI-REC = 275
      *          ADDED RI-LTOUCH-DATE
      *          CHANGED DATES TO CCYYMMDD --- PIC 9(8) COMP-3
      *          CHANGED AMOUNTS FROM PIC S9(7)V99 TO S9(9)V99 COMP-3
      *
      ******************************************************************
       78  RI-SIZE                                   VALUE 275.
       01  RI-REC.
           03  RI1-KEY.
               05  RI-VENDOR      PIC 9(6).
               05  RI-INVOICE     PIC X(10).
      *>>XDD DATE=YYYYMMDD
           03  RI-LTOUCH-DATE     PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  RI-START-DATE      PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  RI-EXPIRE-DATE     PIC 9(8)     COMP-3.
           03  RI-EXPIRE-TIMES    PIC 999      COMP-3.
           03  RI-TIMES-REL       PIC 999      COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  RI-LAST-DATE-REL   PIC 9(8)     COMP-3.
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
           03  FILLER             PIC X(65).
