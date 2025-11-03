      * COPYMEMBER: LIBEX/EX01XLTP
      **************************************************************************
      * PAYMENT SCHEDULE TRAILER EXTRACTION FILE          
      *
      * DATA WAREHOUSE EXTRACTION, CENTRIX PR# 2356
      *
      * REV:
      *   040907 CS  ADD XLTP-REGION-NUMBER [PR#76 REGACC]
      *   180510 CS  ADD BACK XLTP-UNUSED TO MATCH A15 OUTPUT
      *   180510 CS  ADD BACK XLTP-CODE,XLTP-TYPE TO MATCH A15
      **************************************************************************
       78  XLTP-SIZE                    VALUE 229.
       01  XLTP-REC.
           03  XLTP-BRACCTNO            PIC 9(10).
           03  XLTP-ACCTNO              PIC 9(08).
           03  XLTP-CODE                PIC X.
           03  XLTP-SEQNO               PIC 9(03).
           03  XLTP-TYPE                PIC 9(2).

           03  XLTP-SCHLD-TAB.
               05  XLTP-SCHLD-TERM1      PIC 999.
               05  XLTP-SCHLD-PAYMNT1    PIC 9(6).99.
               05  XLTP-SCHLD-TERM2      PIC 999.
               05  XLTP-SCHLD-PAYMNT2    PIC 9(6).99.
               05  XLTP-SCHLD-TERM3      PIC 999.
               05  XLTP-SCHLD-PAYMNT3    PIC 9(6).99.
               05  XLTP-SCHLD-TERM4      PIC 999.
               05  XLTP-SCHLD-PAYMNT4    PIC 9(6).99.
               05  XLTP-SCHLD-TERM5      PIC 999.
               05  XLTP-SCHLD-PAYMNT5    PIC 9(6).99.
               05  XLTP-SCHLD-TERM6      PIC 999.
               05  XLTP-SCHLD-PAYMNT6    PIC 9(6).99.
               05  XLTP-SCHLD-TERM7      PIC 999.
               05  XLTP-SCHLD-PAYMNT7    PIC 9(6).99.
               05  XLTP-SCHLD-TERM8      PIC 999.
               05  XLTP-SCHLD-PAYMNT8    PIC 9(6).99.
               05  XLTP-SCHLD-TERM9      PIC 999.
               05  XLTP-SCHLD-PAYMNT9    PIC 9(6).99.
               05  XLTP-SCHLD-TERM10     PIC 999.
               05  XLTP-SCHLD-PAYMNT10   PIC 9(6).99.
               05  XLTP-SCHLD-TERM11     PIC 999.
               05  XLTP-SCHLD-PAYMNT11   PIC 9(6).99.
               05  XLTP-SCHLD-TERM12     PIC 999.
               05  XLTP-SCHLD-PAYMNT12   PIC 9(6).99.
               05  XLTP-SCHLD-TERM13     PIC 999.
               05  XLTP-SCHLD-PAYMNT13   PIC 9(6).99.
               05  XLTP-SCHLD-TERM14     PIC 999.
               05  XLTP-SCHLD-PAYMNT14   PIC 9(6).99.
               05  XLTP-SCHLD-TERM15     PIC 999.
               05  XLTP-SCHLD-PAYMNT15   PIC 9(6).99.
               05  XLTP-SCHLD-TERM16     PIC 999.
               05  XLTP-SCHLD-PAYMNT16   PIC 9(6).99.
               05  XLTP-SCHLD-TERM17     PIC 999.
               05  XLTP-SCHLD-PAYMNT17   PIC 9(6).99.
           03  XLTP-SCHLD-TABLE REDEFINES XLTP-SCHLD-TAB
                                         OCCURS 17 TIMES.
               05  XLTP-SCHLD-TERM       PIC 999.
               05  XLTP-SCHLD-PAYMNT     PIC 9(6).99.

           03  XLTP-REGION-NUMBER       PIC 9(2).
           03  XLTP-UNUSED              PIC X(48).
           03  XLTP-CR                  PIC X(01).
           03  XLTP-LF                  PIC X(01).
