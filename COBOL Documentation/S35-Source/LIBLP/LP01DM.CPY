      * COPYMEMBER: LIBLP/LP01DM
      **************************************************
      *             ( DEALER MEMO FILE )
      *
      *    DM1-KEY  - DM-BRNO, DM-DL-DLNO, DM-DATE-REVERSED, DM-SEQ
      *
      * PRIORITY MEMOS ; DM-DATE-REVERSED = 0
      * DATED    MEMOS ; DM-DATE-REVERSED = 79809684 (99999999 - 20190315)
      *
      * REV:
      *  JTG 102892 ADDED DL-MEMO-TYPE
      *  GLF 110498 MADE DATE 2000 COMPATIBLE
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      *  CS  990316 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 990510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 990805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 990811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 000410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  BAH 160217 EXPANDED, PD0003
      *  BAH 181015 MOVED DM-BRNO UNDER DM1-KEY 
      *  BAH 190315 MADE DM-DATE REDEFINE DM-DATE-REVERSED
      *   CS 190422 ADD DM-FUTURE-FLAG
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      *  BAH 20230600 ADDED FILLER
      **************************************************
       78  DM-SIZE                     VALUE 115.
       01  DM-REC.
           03  DM1-KEY.
               05  DM-BRNO             PIC 9(4).
               05  DM-DL-DLNO          PIC 9(4)V99.
      * CCYYMMDD SUBTRACTED FROM 99999999
               05  DM-DATE-REVERSED    PIC 9(8).
               05  DM-DATE REDEFINES DM-DATE-REVERSED
                                       PIC 9(8).
               05  DM-SEQ              PIC 9(3).
      $XFD DATE=YYYYMMDD
           03  DM-LTOUCH-DATE          PIC 9(8) COMP-3.
           03  DM-LTOUCH-TIME-HHMM     PIC 9(4) COMP-3.
           03  DM-LTOUCH-USERID        PIC X(10).
           03  DM-MEMO                 PIC X(60).
           03  DM-MEMO-TYPE            PIC XX.
           03  FILLER                  PIC X(14).
