      * COPYMEMBER: LIBAP/AP01AT
      ********************************************************************
      *    ACCTS PAYABLE TRANSACTION HISTORY DETAIL
      *    KEY1 = VENDOR/INVOICE/INVOICE SUFF/PAY SUFF
      *    KEY2 = VENDOR/CHECKNO      
      *
      * REV:
      * BLV 020726 ADDED AT-AHI-DIRECT-SEQ TO HELP WITH PROBLEMS MATCHING
      *            UP, IN CKVOID FOR EXAMPLE
      * BLV 020726 CHANGED NAME OF FIELD AT-AH-SEQ TO AT-AHP-SEQ - IT WAS
      *            TOO CONFUSING!
      ********************************************************************
       78  AT-SIZE                    VALUE 130.
       01  AT-REC.
           03  AT2-KEY.
               05  AT-VENDOR2         PIC 9(6).
               05  AT-CKNO            PIC 9(6).
               05  AT1-KEY.
                   07  AT-VENDOR          PIC 9(6).
                   07  AT-INVOICE         PIC X(16).
                   07  AT-SUFFIX          PIC 99.
                   07  AT-PAY-SUFFIX      PIC 9(4).
      *>>XDD DATE=YYYYMMDD
           03  AT-LTOUCH-DATE         PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  AT-TRANS-CHECKDATE     PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  AT-TRANSDATE           PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  AT-CHECKDATE           PIC 9(8)     COMP-3.
           03  AT-PAYMENT             PIC S9(7)V99 COMP-3.
           03  AT-DISCOUNT            PIC S9(7)V99 COMP-3.
           03  AT-VENDOR-INVOICED     PIC 9(6)     COMP-3.
           03  AT-VEN-INV-ALP REDEFINES AT-VENDOR-INVOICED
                                      PIC X(4).
           03  AT-LOC                 PIC 9(4).
           03  AT-LOC-ALP REDEFINES AT-LOC PIC X(4).
           03  AT-TYPE                PIC X.
           03  AT-AHP-SEQ             PIC 9(3) COMP-3.
           03  AT-AHI-SEQ             PIC 9(3) COMP-3.
           03  FILLER                 PIC X(47).
