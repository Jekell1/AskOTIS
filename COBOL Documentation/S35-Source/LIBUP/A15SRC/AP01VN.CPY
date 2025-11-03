      * COPYMEMBER: LIBAP/AP01VN
      ***********************************************************************
      *  VENDOR MASTER FILE
      *  KEY 1 - VENDOR NUMBER
      *  KEY 2 - ALPHA SORTKEY
      *  REV:
      *  BLV 111400 REDEFINES VN-LABEL-PRINT AS VN-NAME-FLAG, WHICH WILL
      *             INDICATE AN EXTENDED VENDOR NAME IN THE FIRST 2
      *             VENDOR PRIORITY MEMOS
      *  BLV 111400 WE CHANGED THE MEANING OF VN-MTD, VN-YTD,VN-LYTD: THEY
      *             USED TO REPRESENT PURCHASED (IE ACCRUED OR DIRECT
      *             INVOICE POSTED), FOR ACCURATE 1099 REPORTING WE CHANGE
      *             MEANING & UPDATE TO REFLECT PAID, IE CHECK RELEASED
      *  BLM 100817 ADDED VN-ONE-INV-PER-CK.  FLAG WILL BE CHECKED IN AUTO
      *             RELEASE, IF 'Y', WILL RELEASE EACH INVOICE FOR VENDOR
      *             IN SEPARATE RECORD IN AKFILE, SO WILL ONLY PRINT CK
      *             FOR ONE INVOICE.  NOTE, WILL REQUIRE MANUAL RELEASE
      *             OF OFFSETTING CREDITS SINCE NEGATIVE INVOICES CAN'T
      *             BE RELEASED INDIVIDUALLY (WOULD CAUSE NEGATIVE CHK).
      *             WORLD PR# 706
      ***********************************************************************
       01  VN-REC.
           03  VN1-KEY.
               05  VN-NUMBER   PIC 9(6).
           03  VN-NAME         PIC X(25).
           03  VN-ADR1         PIC X(25).
           03  VN-ADR2         PIC X(25).
           03  VN-CITY         PIC X(15).
           03  VN-STATE        PIC X(4).
           03  VN-ZIP          PIC X(10).
           03  VN-TERMS        PIC 99.
           03  VN-DISC         PIC S99V99   COMP-3.
           03  VN-LCHECK       PIC 9(6)     COMP-3.
           03  VN-LCDATE       PIC 9(6)     COMP-3.
           03  VN-LAMT         PIC S9(7)V99 COMP-3.
           03  VN-SUBST        PIC 9(6).
           03  VN-APBAL        PIC S9(7)V99 COMP-3.
           03  VN-MTD          PIC S9(7)V99 COMP-3.
           03  VN-YTD          PIC S9(7)V99 COMP-3.
           03  VN-LYTD         PIC S9(7)V99 COMP-3.
           03  VN-STOP         PIC X.
           03  VN-CLASS        PIC 99.
           03  VN2-KEY.
               05  VN-SORTKEY  PIC X(10).
           03  VN-CUSNO        PIC X(12).
           03  VN-CONTACT      PIC X(25).
           03  VN-PHONE        PIC 9(10)    COMP-3.
           03  VN-FAX          PIC 9(10)    COMP-3.
           03  VN-REMIT-TO     PIC 9(6).
           03  VN-ALLOC-ID     PIC X(16).
           03  VN-1099-FLAG    PIC X.
           03  VN-1099-ID      PIC X(12).
           03  VN-LOC          PIC 9(4).
           03  VN-AUTH         PIC X(10).
           03  VN-VOUCHER-CODE PIC X.
           03  VN-MESSG        PIC X(20).
           03  VN-RELEASED     PIC S9(7)V99 COMP-3.
           03  VN-RELEASED-REDEFINED REDEFINES VN-RELEASED
                               PIC X(5).
           03  VN-LABEL-PRINT  PIC X.
           03  VN-NAME-FLAG    REDEFINES VN-LABEL-PRINT PIC X.
           03  VN-1099-FIXED   PIC X.
           03  VN-ONE-INV-PER-CK PIC X.
           03  VN-UNUSED       PIC X(42).
