      * COPYMEMBER: LIBLP/LP01PA
      ***********************************************************************
      *  PAYEE FILE (FOR MISC. CHECK WRITING)
      *  KEY 1 - PAYEE NUMBER
      *  KEY 2 - ALPHA SORTKEY
      *  REV:
      *  BLM 090526 ADD PA-VAT-PERC AND PA-RFC (WORLD MEXICO TAX ID),
      *             WORLD PR# 638
      *  BLM 120307 ADD PA-CURP FOR WORLD MEXICO, NEED FOR PAYMENT FOR
      *             REFERRALS, WORLD PR# 795
      ***********************************************************************
       78  PA-SIZE             VALUE 315.
       01  PA-REC.
           03  PA1-KEY.
               05  PA-NUMBER   PIC 9(6).
           03  PA-NAME         PIC X(25).
           03  PA-ADR1         PIC X(25).
           03  PA-ADR2         PIC X(25).
           03  PA-CITY         PIC X(15).
           03  PA-STATE        PIC X(2).
           03  PA-ZIP          PIC X(10).
           03  PA-LCHECK       PIC 9(6)     COMP-6.
           03  PA-LCDATE       PIC 9(6)     COMP-6.
           03  PA-LAMT         PIC S9(7)V99 COMP-3.
           03  PA-YTD-PAID     PIC S9(7)V99 COMP-3.
           03  PA-LYTD-PAID    PIC S9(7)V99 COMP-3.
           03  PA2-KEY.
               05  PA-SORTKEY  PIC X(10).
           03  PA-CUSNO        PIC X(12).
           03  PA-CONTACT      PIC X(25).
           03  PA-PHONE        PIC 9(10)    COMP-6.
           03  PA-FAX          PIC 9(10)    COMP-6.
           03  PA-1099-FLAG    PIC X.
           03  PA-1099-ID      PIC X(12).
           03  PA-LEGAL-NAME1  PIC X(33).
           03  PA-LEGAL-NAME2  PIC X(33).
           03  PA-VAT-PERC     PIC 99V99    COMP-6.
           03  PA-RFC          PIC X(13).
           03  PA-CURP         PIC X(18).
           03  PA-UNUSED       PIC X(17).
