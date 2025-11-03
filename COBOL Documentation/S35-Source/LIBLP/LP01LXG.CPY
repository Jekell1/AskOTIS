      * COPYMEMBER: LIBLP/LP01LXG
      ******************************************************************
      *
      * LOAN PAYMENT TRAILER GENERAL LEDGER DIST FILE:
      *
      *    LXG1-KEY = LXG-BRNO, LXG-ACCTNO, LXG-SEQNO
      *
      * CODE:
      *=================================================================
      * REV:
      *  BAH 190115 SPLIT OUT FROM LXFILE
      *  CS 190502 TALKED WITH BETH, THE LX,LXE,LXG,LN,PD,LP ARE PASSED VIA
      *            LP01WK, A LOT OF EFFORT WAS PUT IN TO MAKE THE WORK RECORD
      *            ACCURATE, SO... KEEP THE FILE SIZES THE SAME SIZE AS THE
      *            WORK RECORD.
      *  CS 190516 CHANGE OCCURS FROM GROUP LEVEL ON FUTURE FIELDS
      *  BAH 20200324 CLEANED UP FUTURE FIELDS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      *  BAH 20230600 ADDED FILLER
      **************************************************************************
       78  LXG-SIZE                                  VALUE 300. 
       01  LXG-REC.
           03  LXG1-KEY.
               05  LXG-BRNO         PIC 9(4).
               05  LXG-ACCTNO       PIC 9(8).
               05  LXG-SEQNO        PIC 9(5).
      $XFD DATE=YYYYMMDD
           03  LXG-LTOUCH-DATE      PIC 9(8) COMP-3.

      **************************************************
      *    G - GENERAL LEDGER DISTRIBUTION RECORD:
      **************************************************
           03  LXG-GLTBL           OCCURS 3.
               05  LXG-GLNO         PIC 9(11)     COMP-3.
               05  LXG-GLAMT        PIC S9(7)V99  COMP-3.
           03  FILLER                 PIC X(245).
