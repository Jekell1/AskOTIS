      * COPYMEMBER: LIBLP/LP01LTD
      ******************************************************************
      *    LOAN DEBIT CARD TRAILER FILE 
      *      F4- DEBIT CARD PROCEEDS LONPD0 DISBURSEMENTS
      *
      *    LTD1-KEY = LTD-BRNO, LTD-ACCTNO, LTD-SEQNO
      *
      *---------------------------------------------------------------
      *
      * BAH 02/20/2018 NEW NETSPEND WORLD PR#1306
      *  CS 190507 ADD FUTURE FIELDS
      *  BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      *  BAH 20230600 ADDED FILLER
      **************************************************************************
       78  LTD-SIZE                      VALUE 110.
       01  LTD-REC.
           03  LTD1-KEY.
               05  LTD-BRNO              PIC 9(4).
               05  LTD-ACCTNO            PIC 9(8).
               05  LTD-SEQNO             PIC 999.

      $XFD DATE=YYYYMMDD
           03  LTD-LTOUCH-DATE           PIC 9(8)     COMP-3.
 
           03  LTD-DEBIT-CARD-NUMBER     PIC 9(10)    COMP-3.
           03  LTD-DEBIT-CARD-AMT        PIC S9(7)V99 COMP-3.
      $XFD DATE=YYYYMMDD
           03  LTD-LOANDATE              PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  LTD-VOID-DATE             PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  LTD-CLEARED-DATE          PIC 9(8)     COMP-3.
           03  LTD-CLEARED-AMT           PIC S9(7)V99 COMP-3.
           
           03  FILLER                 PIC X(59).
