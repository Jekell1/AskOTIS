      * COPYMEMBER: LIBLP/LP01LM
      ***********************************************************
      *        LOAN MEMO FILE          BYTES = 80
      *        KEY 1 - ACCTNO, DATE, SEQ
      * REV:
      * BLV 030498 REDEFINE LM-MEMO2 WITH GIFTNET SHIP-TO FIELDS
      * GLF 110498 MADE DATE 2000 COMPATIBLE
      * MJD 990126 ----- UPDATED IN FLEXGEN -----
      * CS  990316 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 990510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 990805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 990811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 000410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * BLM 110330 ADDED REDEF OF LM-ACCTNO, TIRED OF NOT 
      *            HAVING LM-NUMBER!
      ***********************************************************
       01  LM-REC.
           03  LM1-KEY.
               05  LM-ACCTNO             PIC 9(8).
               05  FILLER REDEFINES LM-ACCTNO.
                   07  LM-NUMBER         PIC 9(6).
                   07  LM-LN-SEQ         PIC 99.
               05  LM-DATE               PIC 9(8).
               05  FILLER REDEFINES LM-DATE.
                   07  LM-CC             PIC 99. 
                   07  LM-YY             PIC 99. 
                   07  LM-MM             PIC 99. 
                   07  LM-DD             PIC 99. 
               05  LM-SEQ                PIC 99.
           03  LM-MEMO.
               05  LM-MEMO1              PIC X(30).
               05  LM-MEMO2              PIC X(30).
               05  LM-GIFTNET-SHIPINFO REDEFINES LM-MEMO2.
                   07  LM-GIFTNET-CITY   PIC X(15).
                   07  FILLER            PIC X.
                   07  LM-GIFTNET-STATE  PIC XX.
                   07  FILLER            PIC X.
                   07  LM-GIFTNET-ZIP    PIC X(10).
                   07  FILLER            PIC X.
           03  LM-MEMOX                 REDEFINES LM-MEMO.
               05  LM-FEE                PIC XXX.
               05  FILLER                PIC X.
               05  LM-FEENUM             PIC 9.
               05  FILLER                PIC XX.
               05  LM-FEEDESC            PIC X(15).
               05  FILLER                PIC X(38).
           03  LM-PLACEHOLDER-MEMO      REDEFINES LM-MEMO.
               05  FILLER                PIC X(9).
               05  LM-MEMO-PLACEHOLDER   PIC X(37).
               05  LM-PLACEHOLDER-COLLID PIC X(3).
               05  FILLER                PIC X(11).
           03  LM-MEMO-TYPE              PIC X.
               88  LM-MEMO-TYPE-AUTOINS  VALUE "A".
               88  LM-MEMO-TYPE-BANKRPT  VALUE "B".
               88  LM-MEMO-TYPE-COLLECT  VALUE "C".
               88  LM-MEMO-TYPE-DEBTAID  VALUE "D".
               88  LM-MEMO-TYPE-JD       VALUE "J".
               88  LM-MEMO-TYPE-LEGAL    VALUE "L".
               88  LM-MEMO-TYPE-NOTES    VALUE "N".
               88  LM-MEMO-TYPE-BACKDATE-POFF VALUE "P".
               88  LM-MEMO-TYPE-REVERSAL VALUE "R".
               88  LM-MEMO-TYPE-SYSTEM   VALUE "S" "9".
               88  LM-MEMO-TYPE-TITLE    VALUE "T".
               88  LM-MEMO-TYPE-REPO     VALUE "X".
               88  LM-MEMO-TYPE-UPLOAD   VALUE "U".
               88  LM-MEMO-TYPE-LOAN     VALUE " ".
           03  FILLER                    PIC X(1).
