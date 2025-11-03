      * COPYMEMBER: LIBEX/EX01XLTD
      ******************************************************************
      *    LOAN DEBIT CARD PROCEEDS TRAILER FILE         

      * NEW 180511 CS NETSPEND DATA WAREHOUSE EXTRACT
      **************************************************************************
       78  XLTD-SIZE                      VALUE 107.
       01  XLTD-REC.
           03  XLTD-BRACCTNO             PIC 9(10).
           03  XLTD-ACCTNO               PIC 9(8).
           03  XLTD-CODE                 PIC X.
           03  XLTD-SEQNO                PIC 999.
           03  XLTD-TYPE                 PIC 9(2).
      *>>XDD DATE=YYYYMMDD
 
           03  XLTD-DEBIT-CARD-NUMBER     PIC 9(10).
           03  XLTD-DEBIT-CARD-AMT        PIC +9(7).99.          
      *>>XDD DATE=YYYYMMDD
           03  XLTD-LOANDATE              PIC 9(8).
      *>>XDD DATE=YYYYMMDD
           03  XLTD-VOID-DATE             PIC 9(8).
      *>>XDD DATE=YYYYMMDD
           03  XLTD-CLEARED-DATE          PIC 9(8).
           03  XLTD-CLEARED-AMT           PIC +9(7).99. 
           03  XLTD-REGION-NUMBER         PIC 9(2).
           03  XLTD-UNUSED                PIC X(26).
           03  XLTD-CR                    PIC X.
           03  XLTD-LF                    PIC X.
