      * COPYMEMBER: LIBLP/LP01CA
      **************************************************
      *             ( CASH DRAWER FILE )
      *
      *    KEY 1   - CA-BRNO, CA-DATE, CA-TILLNO
      *
      * REV:
      *  JTG 112097 EXPANDED BY 15 BYTES
      *   CS 112497 ADDED LOGIN ID FOR OPENING OF CASH DRAWER
      *  BAH 062498 CHANGED CA-TILLNO TO ALPHANUMERIC, AND MADE THE FIRST
      *             CHARACTER CA-TILLNO-ZERO SO THE OLD TILLNO'S WOULD
      *             STILL BE IN THE CORRECT POSITION, THE OLD TILLNO WAS 99.
      *             MUST ALWAYS MOVE 0 TO CA-TILLNO-ZERO SO THE OLD RECORDS
      *             MATCH THESE NEW RECORDS.
      *  BLV 980923 ADDED CA-NET-CASH-NOTES TO TRACK CASH DRAWER TRANS
      *             FROM NOTES PAYABLE
      *  MJD 990125 ----- UPDATED IN FLEXGEN -----
      *  CS  1999.0312 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  GLB 990715 ADD CA-PASSWORD-DRAWER PIC X(3), REGMGM PR# 901
      *  KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  BAH 2016.0304 CHANGED TILLNO TO 9(2), ADDED FILLER, EXPANDED DATE
      *  BLM 160915 ADDED CA-CHECK-DEPOSIT FOR SCANNED
      *             CHECKS, WORLD PR# 1188
      *   CS 190415 ADD FUTURE FIELDS, REMOVE ENDING FILLER
      *   CS 190426 REMOVE FUTURE-DATES
      *  BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *   CS 210423 REMOVED FUTURE-FIELDS, FIXED CA-SIZE
      *  BAH 20230600 ADDED FILLER
      **************************************************
       78  CA-SIZE                     VALUE 200.
       01  CA-REC.
           03  CA1-KEY.
               05  CA-BRNO             PIC 9(4).
      $XFD DATE=YYYYMMDD
               05  CA-DATE             PIC 9(8).
      * CA-TILLNO IS NUMERIC BUT STILL WILL CONVERT TO ALPHA SINGLE
      * CHARACTER FOR LACK OF ROOM ON POSTING SCREEN
               05  CA-TILLNO           PIC 99.
      $XFD DATE=YYYYMMDD
           03  CA-LTOUCH-DATE          PIC 9(8)     COMP-3.
           03  CA-DESC                 PIC X(30).
           03  CA-BEGBAL               PIC S9(7)V99 COMP-3.
           03  CA-OPENED-HHMM          PIC 9(4)     COMP-3.
           03  CA-CLOSED-HHMM          PIC 9(4)     COMP-3.
           03  CA-ENDBAL               PIC S9(7)V99 COMP-3.
           03  CA-BALBY                PIC X(10).
           03  CA-RUNNING              PIC S9(7)V99 COMP-3.
           03  CA-DEPOSIT              PIC S9(7)V99 COMP-3.
           03  CA-OPEN-USERID          PIC X(8).
           03  CA-NET-CASH-NOTES       PIC S9(7)V99 COMP-3.
           03  CA-PASSWORD             PIC X(4).
           03  CA-CHECK-DEPOSIT        PIC S9(7)V99 COMP-3.
           03  FILLER                  PIC X(93).
