      * COPYMEMBER: LIBLP/LP01DM
      **************************************************
      *             ( DEALER MEMO FILE )
      *    KEY 1 - DEALER NUMBER, DATE, SEQUENCE
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
      *
      **************************************************
       78  DM-SIZE                     VALUE 140.
       01  DM-REC.
           03  DM-BRNO                 PIC 9(4).
           03  DM1-KEY.
               05  DM-DL-NUM           PIC 9(6).
               05  DM-DL-RED REDEFINES DM-DL-NUM.
                   07  DM-DL-NO        PIC 9(4).
                   07  DM-DL-BR        PIC 9(2).
               05  DM-DATE             PIC 9(8).
               05  FILLER REDEFINES DM-DATE.
                   07  DM-CC           PIC 99.
                   07  DM-YY           PIC 99.
                   07  DM-MM           PIC 99.
                   07  DM-DD           PIC 99.
               05  DM-SEQ              PIC 9(3).
      *>>XDD DATE=YYYYMMDD
           03  DM-LTOUCH-DATE          PIC 9(8) COMP-3.
           03  DM-LTOUCH-TIME-HHMM     PIC 9(4) COMP-3.
           03  DM-LTOUCH-USERID        PIC X(10).
           03  DM-MEMO                 PIC X(60).
           03  DM-MEMO-TYPE            PIC XX.
           03  FILLER                  PIC X(39).
