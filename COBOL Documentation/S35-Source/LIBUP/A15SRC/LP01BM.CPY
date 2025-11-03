      * COPYMEMBER: LIBLP/LP01BM
      **************************************************
      *          ( BORROWER MEMO INDEX # 1 )
      *        ( KEY: BORROWER SSNO/DATE/SEQ  )
      * REV:
      * GLF 110498 CHANGED DATE FOR 2000 COMPATIBILITY
      * MJD 990125 ----- UPDATED IN FLEXGEN -----
      * KEC 990316 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 990510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 990805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 990811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 000410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * MJD 000428 ADDED REDEFINITION OF MEMO FOR PHONE INFO (TFC PR#37)
      **************************************************
       78  BM-SIZE                     VALUE 85.
       01  BM-REC.
           03  BM1-KEY.
               05  BM-SSNO             PIC 9(9).
               05  BM-DATE             PIC 9(8).
               05  FILLER REDEFINES BM-DATE.
                   07  BM-CC           PIC 99.
                   07  BM-YY           PIC 99.
                   07  BM-MM           PIC 99.
                   07  BM-DD           PIC 99.
               05  BM-SEQ              PIC 9(2).
           03  BM-MEMO                 PIC X(60).
           03  BM-MEMO-PHONE REDEFINES BM-MEMO.
               05  BM-PHONE-DESC       PIC X(46).
               05  BM-PHONE-NO         PIC 9(10).
               05  BM-PHONE-EXT        PIC 9(04).
           03  BM-MEMO-TYPE            PIC X.
           03  FILLER                  PIC X(5).
