      * COPYMEMBER: LIBLP/LP01LM
      ***********************************************************
      *        LOAN MEMO FILE         
      *
      *    LM1-KEY = LM-BRNO, LM-ACCTNO, LM-DATE-REVERSED, LM-SEQ
      *
      *  PRIORITY MEMOS ALWAYS DISPLAY FIRST
      *  FOLLOWED BY DATED MEMOS, MOST RECENT FIRST
      *
      * PRIORITY MEMOS ; LM-DATE-REVERSED = 0
      * DATED    MEMOS ; LM-DATE-REVERSED = 79809684 (99999999 - 20190315)
      *                                     79809683 (99999999 - 20190316)
      *
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
      * BAH 160217 EXPANDED RECORD, ADDED LTOUCH, BRNO, PD0003
      * BAH 181127 MOVED LM-BRNO INTO LM1-KEY
      * KEC 190312 CHANGED LM-MEMO TO BE PIC X(60) THEN REDEFINES
      *            IT INTO LM-MEMO1 & LM-MEMO2 FOR X(30).
      * BAH 190315 MADE LM-DATE REDEFINE LM-DATE-REVERSED
      *  CS 190501 ADD LM-FUTURE-FLAG
      *  CS 190612 CHANGE SIZE 
      *  BAH 20210427 REMOVED FUTURE FIELDS
      *  BAH 20230600 ADDED FILLER
      ***********************************************************
       78  LM-SIZE                       VALUE 130.
       01  LM-REC.
           03  LM1-KEY.
               05  LM-BRNO               PIC 9(4).
               05  LM-ACCTNO             PIC 9(8).
      * CCYYMMDD SUBTRACTED FROM 99999999 
               05  LM-DATE-REVERSED      PIC 9(8).
               05  LM-DATE REDEFINES LM-DATE-REVERSED
                                         PIC 9(8).
               05  LM-SEQ                PIC 9(3).
      $XFD DATE=YYYYMMDD
           03  LM-LTOUCH-DATE            PIC 9(8) COMP-3.
           03  LM-LTOUCH-TIME-HHMM       PIC 9(4) COMP-3.
           03  LM-LTOUCH-USERID          PIC X(10).

           03  LM-MEMO                   PIC X(60).
           03  LM-MEMO-SPLIT REDEFINES LM-MEMO.
               05  LM-MEMO1              PIC X(30).
               05  LM-MEMO2              PIC X(30).

           03  LM-MEMO-TYPE              PIC XX.
               88  LM-MEMO-TYPE-AUTOINS  VALUE "A ".
               88  LM-MEMO-TYPE-BANKRPT  VALUE "B ".
               88  LM-MEMO-TYPE-COLLECT  VALUE "C ".
               88  LM-MEMO-TYPE-DEBTAID  VALUE "D ".
               88  LM-MEMO-TYPE-JD       VALUE "J ".
               88  LM-MEMO-TYPE-LEGAL    VALUE "L ".
               88  LM-MEMO-TYPE-NOTES    VALUE "N ".
               88  LM-MEMO-TYPE-BACKDATE-POFF VALUE "P ".
               88  LM-MEMO-TYPE-REVERSAL VALUE "R ".
               88  LM-MEMO-TYPE-SYSTEM   VALUE "S " "9 ".
               88  LM-MEMO-TYPE-TITLE    VALUE "T ".
               88  LM-MEMO-TYPE-REPO     VALUE "X ".
               88  LM-MEMO-TYPE-UPLOAD   VALUE "U.".
               88  LM-MEMO-TYPE-LOAN     VALUE "  ".
           03  FILLER                 PIC X(27).
