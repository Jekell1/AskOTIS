      * COPYMEMBER: LIBLP/LP01LH
      **************************************************
      *        LOAN HISTORY FILE        BYTES = 180
      *        KEY - SSNO, ACCTNO
      * REV:
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      *  BLV 070718 ADDED LH-ARCHIVE-FG, WORLD PR# XXX
      *  BLM 160616 REMOVE LH-SEQNO, CHANGE LH-NUMBER
      *             TO PIC 9(8), PD0003
      *  BAH 180628 REMOVED LH-NUMBER
      **************************************************
       78  LH-SIZE                 VALUE 180.
       01  LH-REC.
           03  LH-BRNO             PIC 9(4).
           03  LH1-KEY.
               05  LH-SSNO         PIC 9(10).
               05  LH-ACCTNO       PIC 9(8).

      *>>XDD DATE=YYYYMMDD
           03  LH-LTOUCH-DATE      PIC 9(8)     COMP-3.
      *---------------------------------------------------
           03  LH-CLASS            PIC 9(3)     COMP-3.
           03  LH-LAWCD            PIC 99.
           03  LH-COLLCD           PIC XX.
           03  LH-MAKERCD          PIC X.
           03  LH-POCD             PIC X(3).
      *---------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  LH-LOANDATE         PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LH-PLDATE           PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LH-JDDATE           PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LH-POFFDATE         PIC 9(8)     COMP-3.
           03  LH-LNAMT            PIC S9(7)V99 COMP-3.
           03  LH-ORGTERM          PIC 9(3)     COMP-3.
           03  LH-POTERMS          PIC 9(3)     COMP-3.
           03  LH-PYPROFILE        PIC 9(12)    COMP-3.
           03  LH-DLNO             PIC 9(4)V99  COMP-3.
           03  LH-STATUS           PIC X(10).
           03  LH-ARCHIVE-FG       PIC X.

           03  FILLER              PIC X(92).
