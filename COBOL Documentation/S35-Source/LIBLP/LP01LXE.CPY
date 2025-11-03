      * COPYMEMBER: LIBLP/LP01LXE
      ******************************************************************
      *
      * LOAN PAYMENT TRAILER EARNINGS FILE: 
      *
      *   LXE1-KEY = LXE-BRNO, LXE-ACCTNO, LXE-SEQNO
      *
      *  BAH 190115 SPLIT OUT FROM LXFILE
      *  CS 190502 TALKED WITH BETH, THE LX,LXE,LXG,LN,PD,LP ARE PASSED VIA
      *            LP01WK, A LOT OF EFFORT WAS PUT IN TO MAKE THE WORK RECORD
      *            ACCURATE, SO... KEEP THE FILE SIZES THE SAME SIZE AS THE
      *            WORK RECORD.
      *   CS190516 CHANGED OCCURS FROM GROUP LEVEL
      *   CS 190606 NEEDS TO STAY SAME AS WK-LXE
      *  BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      *  BAH 20230600 ADDED FILLER
      **************************************************************************
       78  LXE-SIZE                                VALUE 270.
       01  LXE-REC.
           03  LXE1-KEY.
               05  LXE-BRNO         PIC 9(4).
               05  LXE-ACCTNO       PIC 9(8).
               05  LXE-SEQNO        PIC 9(5).
      $XFD DATE=YYYYMMDD
           03  LXE-LTOUCH-DATE      PIC 9(8) COMP-3.

      **************************************************
      *    E - EARNINGS TRAILER RECORD.
      *                  
      * EARNINGS 4 - 10
      **************************************************
           03  LXE-EARN            PIC S9(7)V99  COMP-3 OCCURS 7.

      * WHEN SP-ERNFRMLA = 15 & 16, THIS FIELD HOLDS IB-EARNINGS
      * INTEREST OWING BEFORE THIS TRANSACTION

           03  LXE-WORKER          PIC S9(7)V99  COMP-3.

      * WHEN SP-ERNFRMLA = 15 & 16, THIS FIELD HOLDS IB-EARNINGS
      * INTEREST TAKEN ON THIS TRANSACTION

           03  LXE-WORKER2         PIC S9(7)V99  COMP-3.
           03  FILLER                 PIC X(203).
