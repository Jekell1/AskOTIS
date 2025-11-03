      * COPYMEMBER: LIBWI/SPSCANW
      * 160805 CHANGED SPSCAN-SP1-KEY TO SPSCAN-SP1-KEY.
      *        CHANGED SPSCAN-SP-SPRCLASS FROM 9(2) TO 9(3)
      * 160816 CHANGED SPSCAN-SPRCLASS FROM 9(2) TO 9(3)
      * 2024.0507 BAH MADE LEVEL 01, CALL NOT WINDOW
       01  SPSCAN-BUF.
           03  SPSCAN-SP1-KEY          PIC X(09).
           03  FILLER        REDEFINES SPSCAN-SP1-KEY.
               05  SPSCAN-SP-ORGST     PIC XX.
               05  SPSCAN-SP-SPRCLASS  PIC 999.
               05  SPSCAN-SP-SUBCLASS  PIC 99.
               05  SPSCAN-SP-LAWCODE   PIC 99.
           03  SPSCAN-VERIFY.
               05  SPSCAN-ORGST        PIC XX.
               05  SPSCAN-SPRCLASS     PIC 999.
               05  SPSCAN-SUBCLASS     PIC 999.
               05  SPSCAN-LAWCODE      PIC 99.
           03  SPSCAN-LAST-LAW         PIC X    VALUE " ".
           03  SPSCAN-SALEFIN          PIC X    VALUE " ".
           03  SPSCAN-STATUS           PIC XX.

