      * COPYMEMBER: LIBWI/SCSCANW
      *******************************************************
      *                 (SCSCANW)
      *    SUBCLASS SCAN WINDOW WORK BUFFER
      * CS 160805 CHANGED SCSCAN-SP-SPRCLASS FROM 9(2) TO 9(3),
      *           SCSCAN-SP-SPRCLASS FROM (2) TO 9(3)
      *           WHY IS SCSCAN-SUBCLASS 9(3)?
      *           CHANGED SCSCAN-KEY TO SCSCAN-SP1-KEY.
      * BAH 2017.0406 CHANGED SCSCAN-TRANS-DATE TO 9(8) FROM 9(6)
      * BAH 2024.0503 MADE LEVEL 01, CALL NOT WINDOW
      *******************************************************
       01  SCSCAN-BUF.
           03  SCSCAN-SP1-KEY.
               05  SCSCAN-SP-ORGST     PIC XX.
               05  SCSCAN-SP-SPRCLASS  PIC 999.
               05  SCSCAN-SP-SUBCLASS  PIC 99.
               05  SCSCAN-SP-LAWCODE   PIC 99.
           03  SCSCAN-SP1-VERIFY.
               05  SCSCAN-ORGST        PIC XX.
               05  SCSCAN-SPRCLASS     PIC 999.
               05  SCSCAN-SUBCLASS     PIC 999.
               05  SCSCAN-LAWCODE      PIC 99.
           03  SCSCAN-LAST-LAW         PIC X    VALUE " ".
           03  SCSCAN-TRANS-DATE       PIC 9(8).
           03  SCSCAN-STATUS           PIC XX.

