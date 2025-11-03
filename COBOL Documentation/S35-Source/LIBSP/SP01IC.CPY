      * COPYMEMBER: LIBSP/SP01IC
      ***********************************************************************
      *  SP01IC - IRS PAYER "B' RECORD FOR 1099-C (CANCELLATION OF DEBT)
      *              CREATED BY SP/LN1099 AND SP/MO1099
      *  REV:
      *  010119 BLV ADDED IC-B-DATE-CANCELED
      *  050728 BLF ADDED IC-B-RECORD-SEQ-NO, IC-B-SPECIAL, & LINE FEEDS
      *  12/30/2016 BAH IC-B-PAYAMT NOW OCCURS 16 TIMES, NOT 12
      *  10/08/2017 BAH CHANGED IC-B-DATE-CANCEL-MM,DD,YY TO
      *                 IC-B-DATE-CANCEL-YYYYMMDD  PIC 9(8).
      *  01/11/2022 BAH IC-B-PAYAMT OCCURS 2 MORE TIMES, 18 INSTEAD OF 16.
      *                 REMOVED FILLER BETWEEN 2ND-PAYEE-NAME AND PAYEE-ADDR
      *                 ADDED MORTGAGE ACQUISITION-DATE AT POSITION 723 #1537
      ***********************************************************************

       01  IC-REC.
           03  IC-TYPE                    PIC X.
           03  FILLER                     PIC X(747).
           03  IC-CARR-RTN                PIC X.
           03  IC-LINE-FEED               PIC X.

       01  IC-B-REC.
           03  FILLER                     PIC X.
           03  IC-B-PAY-YEAR              PIC 9(4).
           03  IC-B-CORRECTED-RTNCD       PIC X.
           03  IC-B-NAME-CONTROL          PIC X(4).
           03  IC-B-TIN-TYPE-CD           PIC X.
      * 12-20
           03  IC-B-TIN                   PIC 9(9).
           03  IC-B-ACCTNO                PIC X(20).
           03  IC-B-OFFICE-CODE           PIC 9(4).
           03  FILLER                     PIC X(10).
      * 55-270
           03  IC-B-PAYAMT            OCCURS 18 TIMES
                                          PIC S9(10)V99.
      * 271-286
           03  FILLER                     PIC X(16).
      * 287
           03  IC-B-FOREIGN-CD            PIC X.
      * 288
           03  IC-B-PAYEE-NAME            PIC X(40).
      * 328
           03  IC-B-2ND-PAYEE-NAME        PIC X(40).
      * 368
           03  IC-B-PAYEE-ADDR            PIC X(40).
           03  FILLER                     PIC X(40).
      * 448
           03  IC-B-PAYEE-CITY            PIC X(40).
           03  IC-B-PAYEE-STATE           PIC XX.
      * 490-498
           03  IC-B-PAYEE-ZIP             PIC X(9).
           03  FILLER                     PIC X.
      * 500-507
           03  IC-B-RECORD-SEQ-NO         PIC 9(8).
      * 508-543
           03  FILLER                     PIC X(36).
      * 544-546
           03  FILLER                     PIC X(3).
      * 547 IDENTIFIABLE EVENT CODE REQUIRED 
      *     "F" - BY AGREEMENT
           03  IC-B-EVENT-CODE            PIC X.
      * 548-555
           03  IC-B-DATE-CANCEL-YYYYMMDD  PIC 9(8).
      * 556-594
           03  IC-B-DEBT-DESC             PIC X(39).
      * 595
           03  IC-B-PERSONAL-LIABILITY    PIC X.
      * 596-662
           03  FILLER                     PIC X(67).
      * 663-722
           03  IC-B-SPECIAL               PIC X(60).
      * 723-748
           03  FILLER                     PIC X(26).
      * 749-750
           03  IC-B-CARR-RTN              PIC X.
           03  IC-B-LINE-FEED             PIC X.

