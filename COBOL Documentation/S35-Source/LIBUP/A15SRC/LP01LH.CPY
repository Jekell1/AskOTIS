      * COPYMEMBER: LIBLP/LP01LH
      **************************************************
      *        LOAN HISTORY FILE        BYTES = 79
      *        KEY - SSNO, ACCTNO
      * REV:
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      *  BLV 070718 ADDED LH-ARCHIVE-FG, WORLD PR# XXX
      **************************************************
       78  LH-SIZE                 VALUE 79.
       01  LH-REC.
           03  LH1-KEY.
               05  LH-SSNO         PIC 9(10).
               05  LH-ACCTNO       PIC 9(8).
               05  LH-ACCTNOX REDEFINES LH-ACCTNO.
                   07  LH-NUMBER   PIC 9(6).
                   07  LH-SEQNO    PIC 9(2).
      *---------------------------------------------------
           03  LH-CLASS            PIC 99       COMP-3.
           03  LH-LAWCD            PIC 99       COMP-3.
           03  LH-COLLCD           PIC XX.
           03  LH-MAKERCD          PIC X.
           03  LH-POCD             PIC X(3).
      *---------------------------------------------------
           03  LH-LOANDATE         PIC 9(6)     COMP-3.
           03  LH-PLDATE           PIC 9(6)     COMP-3.
           03  LH-JDDATE           PIC 9(6)     COMP-3.
           03  LH-PAIDOFF          PIC 9(6)     COMP-3.
           03  LH-LNAMT            PIC 9(6)V99  COMP-3.
           03  LH-ORGTERM          PIC 9(3)     COMP-3.
           03  LH-POTERMS          PIC 9(3)     COMP-3.
           03  LH-PYPROFILE        PIC 9(12)    COMP-3.
           03  LH-DLNO             PIC 9(4)V99  COMP-3.
      *---------------------------------------------------
           03  LH-STATUS           PIC X(10).
           03  LH-ARCHIVE-FG       PIC X.
           03  FILLER              PIC X(4).
