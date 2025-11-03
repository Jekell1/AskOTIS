      * COPYMEMBER: LIBLP/LP01LTD
      ******************************************************************
      *    LOAN DEBIT CARD TRAILER FILE : (95) BYTES
      *      F4- DEBIT CARD PROCEEDS LONPD0 DISBURSEMENTS
      *
      *---------------------------------------------------------------
      *
      * BAH 02/20/2018 NEW NETSPEND WORLD PR#1306
      **************************************************************************
       78  LTD-SIZE                      VALUE 95.
       01  LTD-REC.
           03  LTD1-KEY.
               05  LTD-BRNO              PIC 9(4).
               05  LTD-ACCTNO            PIC 9(8).
               05  LTD-SEQNO             PIC 999.

      *>>XDD DATE=YYYYMMDD
           03  LTD-LTOUCH-DATE           PIC 9(8)     COMP-3.
 
           03  LTD-DEBIT-CARD-NUMBER     PIC 9(10)    COMP-3.
           03  LTD-DEBIT-CARD-AMT        PIC S9(7)V99 COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LTD-LOANDATE              PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LTD-VOID-DATE             PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LTD-CLEARED-DATE          PIC 9(8)     COMP-3.
           03  LTD-CLEARED-AMT           PIC S9(7)V99 COMP-3.

           03  FILLER                PIC X(44).
