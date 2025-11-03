      * COPYMEMBER: LIBLP/LP01LTF
      ******************************************************************
      *    LOAN FEE CODE TRAILER FILE 
      *
      *    LTF1-KEY = LTF-BRNO, LTF-ACCTNO, LTF-SEQNO
      *
      * FEE CODE PAYABLE (COMMISSION)
      *  CODE   SEQNO   TYPE    DESCRIPTION
      *    F      1     N/A     FEE CODE PAYABLE - SUCH AS NON-FILE
      *
      * BAH 07/01/2015 CREATED NEW
      *  CS 190509 ADD FUTURE FIELDS, ADDED TABLE OF 10 BECAUSE THE LOAN
      *            ALLOWS FOR 20 FEES.
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      *  BAH 20230600 ADDED FILLER
      **************************************************************************
       78  LTF-SIZE                     VALUE 240.
       01  LTF-REC.
           03  LTF1-KEY.
               05  LTF-BRNO                      PIC 9(4).
               05  LTF-ACCTNO                    PIC 9(8).
               05  LTF-SEQNO                     PIC 999.

      $XFD DATE=YYYYMMDD
           03  LTF-LTOUCH-DATE                   PIC 9(8)     COMP-3.

           03  LTF-FEECODE-TBL                OCCURS 10.
               05  LTF-FEECODE                   PIC XX.
               05  LTF-FEECODE-PAYABLE           PIC S9(7)V99 COMP-3.


           03  FILLER                 PIC X(150).
