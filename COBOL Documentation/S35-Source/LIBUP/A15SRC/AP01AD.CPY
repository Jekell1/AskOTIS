      * COPYMEMBER: LIBAP/AP01AD
      **************************************************
      *    ACCTS PAYABLE TRANSACTION RELEASE DETAIL    *
      *    KEY1 = LOC, VENDOR, CHECK SEQ, INVOICE, SUFF*
      **************************************************
       01  AD-REC.
           03  AD1-KEY.
               05  AD-LOC             PIC 9(4).
      * GLB ADD CLASS 11/98  
               05  AD-CLASS           PIC 9(2).
      * GLB EXPAND FORM 6 - 9 11/98  
               05  AD-VENDOR          PIC 9(9).
      * GLF REDEFINE AD-VENDOR WITH AD-INVESTOR
               05  AD-INVESTOR REDEFINES AD-VENDOR PIC 9(9).
               05  AD-CHECK-SEQ       PIC 9(3).
               05  AD-INVOICE         PIC X(16).
               05  AD-SUFFIX          PIC 99.
               05  AD-SEQ             PIC 9(3).
      * GLB EXPAND FORM 6 - 8 11/98  
           03  AD-REV-DATE        PIC 9(8).
           03  AD-BLOC            PIC 9(4).
           03  AD-TYPE            PIC X.
           03  AD-INVDATE         PIC 9(6)     COMP-3.
           03  AD-DUEDATE         PIC 9(6)     COMP-3.
           03  AD-DISC-DATE       PIC 9(6)     COMP-3.
           03  AD-REFNO           PIC X(15).
           03  AD-SPLIT-AMT       PIC S9(7)V99 COMP-3.
           03  AD-PAIDAMT         PIC S9(7)V99 COMP-3.
           03  AD-NONDISC         PIC S9(7)V99 COMP-3.
           03  AD-SPLIT-DISC      PIC S9(5)V99 COMP-3.
           03  AD-DISC-TAKEN      PIC S9(5)V99 COMP-3.
           03  AD-TERMS           PIC 99       COMP-3.
           03  AD-PURORD          PIC X(12).
           03  AD-TOT-PAYS        PIC 99.
           03  AD-MONTHLY-CYCLE   PIC 99.
           03  AD-PAYMENT         PIC S9(7)V99 COMP-3.
           03  AD-DISCOUNT        PIC S9(5)V99 COMP-3.
           03  AD-GL-AREA.
               05  AD-GLAL-AREA    PIC X(64).
               05  AD-ALID-AREA REDEFINES AD-GLAL-AREA.
                   07  AD-ALID     PIC X(16)           OCCURS 4.
               05  AD-ACCOUNT-AREA REDEFINES AD-GLAL-AREA.
                   07  AD-GLNO     PIC 9(11)    COMP-3 OCCURS 10.
                   07  AD-GL-TYPE  PIC X(4).
               05  AD-GLAMT        PIC S9(7)V99 COMP-3 OCCURS 10. 
           03  AD-CHECKNO         PIC 9(6).
           03  AD-VENDOR-INVOICED PIC 9(9)     COMP-3.
      * GLF REDEFINE AD-VENDOR-INVOICED WITH AD-INVESTOR-INVOICED 1/99
           03  AD-INVESTOR-INVOICED REDEFINES AD-VENDOR-INVOICED
                                  PIC 9(9)     COMP-3.
      * GLB    NEW FIELDS         11/98
           03  AD-TRCD            PIC X.
           03  AD-REALTRCD        PIC XX.
           03  AD-SOURCE-CODE     PIC XX.
           03  AD-ACCTNO          PIC 9(6).
           03  AD-NHSEQNO         PIC 9(4).
           03  AD-BATCH-ID        PIC 9(5).
           03  FILLER             PIC X(15).
