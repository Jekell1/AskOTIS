      * COPYMEMBER: LIBSP/SP01IL
      * 012799 BAH EXPANDED RECORD SIZE FROM 420 TO 750
      * 021217 BAH ADDED IL-B-RECORD-SEQ-NO IN MIDDLE OF RECORD
      * 11/22/2016 BAH ADDED NEW FIELDS FROM 544 THRU END OF RECORD
      *                REGENCY PR# 5214
      * 12/30/2016 BAH IL-B-PAYAMT NOW OCCURS 16 TIMES AND MUST BE 0 FILLED
      * 12/21/2017 BAH ADDED IL-B-NO-MORTGAGED-PROP, REGENCY PR#5332
      * 01/11/2022 BAH IL-B-PAYAMT OCCURS 2 MORE TIMES, 18 INSTEAD OF 16.
      *                REMOVED FILLER BETWEEN 2ND-PAYEE-NAME AND PAYEE-ADDR
      *                ADDED MORTGAGE ACQUISITION-DATE AT POSITION 723 #1537
       01  IL-REC.
           03  IL-TYPE                PIC X.
           03  FILLER                 PIC X(747).
           03  IL-CARR-RTN            PIC X.
           03  IL-LINE-FEED           PIC X.

       01  IL-B-REC.
           03  FILLER                 PIC X.
           03  IL-B-PAY-YEAR          PIC 9(4).
           03  IL-B-CORRECTED-RTNCD   PIC X.
           03  IL-B-NAME-CONTROL      PIC X(4).
           03  IL-B-TIN-TYPE-CD       PIC X.
      * 12-20
           03  IL-B-TIN               PIC 9(9).
           03  IL-B-ACCTNO            PIC X(20).
           03  IL-B-OFFICE-CODE       PIC 9(4).
           03  FILLER                 PIC X(10).
      * 55-270
           03  IL-B-PAYAMT            OCCURS 18 TIMES
                                      PIC S9(10)V99.
      * 271-286
           03  FILLER                 PIC X(16).
      * 287
           03  IL-B-FOREIGN-CD        PIC X.
      * 288-327
           03  IL-B-PAYEE-NAME        PIC X(40).
      * 328-367
           03  IL-B-2ND-PAYEE-NAME    PIC X(40).
      * 368-407
           03  IL-B-PAYEE-ADDR        PIC X(40).
      * 408-447
           03  FILLER                 PIC X(40).
      * 448-487
           03  IL-B-PAYEE-CITY        PIC X(40).
      * 488-489
           03  IL-B-PAYEE-STATE       PIC XX.
      * 490-498
           03  IL-B-PAYEE-ZIP         PIC X(9).
           03  FILLER                 PIC X.
      * 500-507
           03  IL-B-RECORD-SEQ-NO     PIC 9(8).
      * 508-543
           03  FILLER                 PIC X(36).
      * 544-551
           03  IL-B-LOANDATE-CCYYMMDD PIC 9(8).
      * 552 
           03  IL-B-PROPERTY-SECURING PIC X.
      * 553-591 
           03  IL-B-PROPERTY-ADDRESS  PIC X(39).
      * 592-630
           03  IL-B-DESC-OF-PROPERTY  PIC X(39).
      * 631-669
           03  IL-B-OTHER             PIC X(39).
      * 670-673
           03  IL-B-NO-MORTGAGED-PROP PIC 9(4).
      * 674-722
           03  IL-B-SPECIAL-DATA      PIC X(49).
      * 723-730
           03  IL-B-MTG-ACQ-DATE-CCYYMMDD  PIC 9(8).
      * 731-748
           03  FILLER                 PIC X(18).
           03  IL-B-CARR-RTN          PIC X.
           03  IL-B-LINE-FEED         PIC X.

