      * COPYMEMBER: LIBLA/LA01AM
      ******************************************************************
      *
      *    LOAN APPLICATION MEMO FILE                         BYTES = 80
      *    KEY 1 - APPNO, APPDATE, SEQ
      *
      *    - AM-APPDATE IS IN FORMAT: YYMMDD SUBTRACT 999999
      * REV:
      * GLF 110498 MADE DATE 2000 COMPATIBLE
      * MJD 990125 ----- UPDATED IN FLEXGEN -----
      * CS  1999.0315 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *
      ******************************************************************
       01  AM-REC.
           03  AM1-KEY.
               05  AM-APPNO              PIC 9(6).
               05  AM-DATE               PIC 9(8).
               05  FILLER REDEFINES AM-DATE.
                   07  AM-CC             PIC 99.
                   07  AM-YY             PIC 99.
                   07  AM-MM             PIC 99.
                   07  AM-DD             PIC 99.
               05  AM-SEQ                PIC 9(2).
           03  AM-MEMO.
               05  AM-MEMO1              PIC X(30).
               05  AM-MEMO2              PIC X(30).
           03  AM-MEMO-TYPE              PIC X(1).
           03  FILLER                    PIC X(3).
