      * COPYMEMBER: LIBSP/SP01IM
      *
      * 012799 BAH EXPANDED RECORD SIZE FROM 420 TO 750, ALONG WITH MANY OTHER
      *            CHANGES, AS PER IRS SPECS...
      * 020104 BAH ADDED VENDOR INFORMATION
      * 021217 BAH IRS ADDED NEW FIELDS IN THE MIDDLE OF THE RECORD, A NEW
      *            REQUIRED RECORD SEQUENCE NUMBER IN EACH RECORD ALSO
      * 070117 BAH/BLF CHANGES FOR REPORTING YEAR 2006, WORLD PR# 518
      * 110120 BAH IM-A-RETURN-TYPE IS NOW 2 CHARACTERS, REMOVED THE FIELD
      *            ABOVE IT (IM-A-RED-STA-COMBO-CD) AND NOW RETURN-TYPE
      *            STARTS IN POSITION 26 INSTEAD OF 27 AND IS LEFT JUSTIFIED
      *            SPACE FILLED PR# 3718
      * 11/22/2016 BAH ADDED NEW FIELDS FROM 544 THRU END OF RECORD
      *                REGENCY PR# 5214
      * 12/30/2016 BAH REMOVED IM-T-TAPE-INDICATOR AND IM-T-TRANS-MEDIA,
      *                IM-B-PAYAMT OCCURS 16 NOW, NOT 12, IM-C-TOTAL OCCURS 16
      *                NOW, NOT 12.
      * 12/21/2017 BAH ADDED IM-B-NO-MORTGAGED-PROP, REGENCY PR#5332
      * 01/11/2022 BAH IM-B-PAYAMT OCCURS 2 MORE TIMES, 18 INSTEAD OF 16.
      *                REMOVED FILLER BETWEEN 2ND-PAYEE-NAME AND PAYEE-ADDR
      *                ADDED MORTGAGE ACQUISITION-DATE AT POSITION 723,
      *                INCREASED IM-A-AMT-INDICATORS FROM 16 TO 18,
      *                INCREASED IM-C-TOTAL FROM 16 TO 18             #1537
       01  IM-REC.
           03  IM-TYPE                PIC X.
           03  FILLER                 PIC X(747).
           03  IM-CARR-RTN            PIC X.
           03  IM-LINE-FEED           PIC X.

       01  IM-T-REC.
           03  FILLER                 PIC X.
           03  IM-T-PAY-YEAR          PIC 9(4).
           03  IM-T-PRIOR-YEAR        PIC X.
      * 7-15
           03  IM-T-FED-EIN           PIC 9(9).
      * 16-20
           03  IM-T-TCC               PIC X(5).
      * 21-27
           03  FILLER                 PIC X(7).
      * 28
           03  IM-T-TEST              PIC X.
           03  IM-T-FOREIGN-ENTITY    PIC X.
      * 30-109
           03  IM-T-TRANS-NAME        PIC X(40).
           03  IM-T-TRANS-NAME2       PIC X(40).
      * 110 - 149
           03  IM-T-COMPANY-NAME      PIC X(40).
           03  IM-T-COMPANY-NAME2     PIC X(40).
      * 190
           03  IM-T-COMPANY-ADDR      PIC X(40).
      * 230-269
           03  IM-T-COMPANY-CITY      PIC X(40).
           03  IM-T-COMPANY-STATE     PIC X(2).
           03  IM-T-COMPANY-ZIPCODE   PIC X(9).
           03  FILLER                 PIC X(15).
      * 296-303
           03  IM-T-NO-B-RECS         PIC 9(8).
      * 304-343
           03  IM-T-CONTACT           PIC X(40).
      * 344-353
           03  IM-T-CONTACT-PHONE     PIC 9(10).
           03  IM-T-CONTACT-EXT       PIC X(5).
      * 359-408
           03  IM-T-CONTACT-EMAIL     PIC X(50).
      * 409-499
           03  FILLER                 PIC X(91).
      * 500-507
           03  IM-T-RECORD-SEQ-NO     PIC 9(8).
           03  FILLER                 PIC X(10).

      * 518
           03  IM-T-VENDOR-FLAG       PIC X.
           03  IM-T-VENDOR-NAME       PIC X(40).
           03  IM-T-VENDOR-ADDR       PIC X(40).
           03  IM-T-VENDOR-CITY       PIC X(40).
      * 639-640
           03  IM-T-VENDOR-STATE      PIC X(2).
           03  IM-T-VENDOR-ZIP        PIC X(9).
           03  IM-T-VENDOR-CONTACT    PIC X(40).
           03  IM-T-VENDOR-PHONE      PIC X(15).
      * 705-739
           03  FILLER                 PIC X(35).

      * 740
           03  IM-T-VENDOR-FOREIGN    PIC X.
      * 741 748
           03  FILLER                 PIC X(8).
           03  IM-T-CARR-RTN          PIC X.
           03  IM-T-LINE-FEED         PIC X.

       01  IM-A-REC.
           03  FILLER                 PIC X.
           03  IM-A-PAY-YEAR          PIC 9(4).
           03  FILLER                 PIC X(6).
      * 12-20 PAYER'S TAXPAYER IDENTIFICATION NUMBER (TIN)
           03  IM-A-FED-EIN           PIC 9(9).
      * 21-24 PAYER NAME CONTROL
           03  IM-A-NAME-CONTROL-NO   PIC X(4).
      * 25 LAST FILING INDICATOR
           03  IM-A-LAST-FILING-FLAG  PIC X.
      * 26-27 TYPE OF RETURN   "3 " 1098
           03  IM-A-RETURN-TYPE       PIC XX.
      * 28-45
           03  IM-A-AMT-INDICATORS    PIC X(18).
      * 46-51
           03  FILLER                 PIC X(6).
      * 52
           03  IM-A-FOREIGN-ENTITY    PIC X.
      * 53-92
           03  IM-A-PAYER-NAME        PIC X(40).
           03  IM-A-2ND-PAYER-NAME    PIC X(40).
           03  IM-A-TRANSFER-AGENT-CD PIC X.
           03  IM-A-PAYER-ADDR        PIC X(40).
      * 174
           03  IM-A-PAYER-CITY        PIC X(40).
           03  IM-A-PAYER-STATE       PIC X(2).
           03  IM-A-PAYER-ZIP         PIC X(9).
           03  IM-A-PAYER-PHONE       PIC 9(10).
           03  IM-A-PAYER-EXT         PIC X(5).
      * 240-499
           03  FILLER                 PIC X(260).
      * 500-507
           03  IM-A-RECORD-SEQ-NO     PIC 9(8).
      * 508-748
           03  FILLER                 PIC X(241).
      * 749-750
           03  IM-A-CARR-RTN          PIC X.
           03  IM-A-LINE-FEED         PIC X.

       01  IM-B-REC.
           03  FILLER                 PIC X.
           03  IM-B-PAY-YEAR          PIC 9(4).
           03  IM-B-CORRECTED-RTNCD   PIC X.
           03  IM-B-NAME-CONTROL      PIC X(4).
           03  IM-B-TIN-TYPE-CD       PIC X.
      * 12-20
           03  IM-B-TIN               PIC 9(9).
           03  IM-B-ACCTNO            PIC X(20).
           03  IM-B-OFFICE-CODE       PIC 9(4).
           03  FILLER                 PIC X(10).
      * 55-270
           03  IM-B-PAYAMT            OCCURS 18 TIMES
                                      PIC S9(10)V99.
      * 271-286
           03  FILLER                 PIC X(16).
      * 287
           03  IM-B-FOREIGN-CD        PIC X.
      * 288-327
           03  IM-B-PAYEE-NAME        PIC X(40).
      * 328-367
           03  IM-B-2ND-PAYEE-NAME    PIC X(40).
      * 368-407
           03  IM-B-PAYEE-ADDR        PIC X(40).
      * 408-447
           03  FILLER                 PIC X(40).
      * 448-487
           03  IM-B-PAYEE-CITY        PIC X(40).
      * 488-489
           03  IM-B-PAYEE-STATE       PIC XX.
      * 490-498
           03  IM-B-PAYEE-ZIP         PIC X(9).
           03  FILLER                 PIC X.
      * 500-507
           03  IM-B-RECORD-SEQ-NO     PIC 9(8).
      * 508-543
           03  FILLER                 PIC X(36).
      * 544-551
           03  IM-B-LOANDATE-CCYYMMDD PIC 9(8).
      * 552
           03  IM-B-PROPERTY-SECURING PIC X.
      * 553-591
           03  IM-B-PROPERTY-ADDRESS  PIC X(39).
      * 592-630
           03  IM-B-DESC-OF-PROPERTY  PIC X(39).
      * 631-669
           03  IM-B-OTHER             PIC X(39).
      * 670-673
           03  IM-B-NO-MORTGAGED-PROP PIC 9(4).
      * 674-722
           03  IM-B-SPECIAL-DATA      PIC X(49).
      * 723-730
           03  IM-B-MTG-ACQ-DATE-CCYYMMDD  PIC 9(8).
      * 731-748
           03  FILLER                 PIC X(18).
           03  IM-B-CARR-RTN          PIC X.
           03  IM-B-LINE-FEED         PIC X.

       01  IM-C-REC.
           03  FILLER                 PIC X.
           03  IM-C-NO-PAYEES         PIC 9(8).
      * 10-15
           03  FILLER                 PIC X(6).
      * 16-339
           03  IM-C-TOTAL             OCCURS 18 TIMES
                                      PIC S9(16)V99.
      * 340 - 499
           03  FILLER                 PIC X(160).
      * 500-507
           03  IM-C-RECORD-SEQ-NO     PIC 9(8).
      * 508-748
           03  FILLER                 PIC X(241).
           03  IM-C-CARR-RTN          PIC X.
           03  IM-C-LINE-FEED         PIC X.

       01  IM-F-REC.
           03  FILLER                 PIC X.
      * 2-9
           03  IM-F-NO-A-RECS         PIC 9(8).
      * 10-30
           03  IM-F-ZEROS             PIC X(21).
           03  FILLER                 PIC X(27).
           03  FILLER                 PIC X(442).
      * 500-507
           03  IM-F-RECORD-SEQ-NO     PIC 9(8).
           03  FILLER                 PIC X(241).
           03  IM-F-CARR-RTN          PIC X.
           03  IM-F-LINE-FEED         PIC X.

