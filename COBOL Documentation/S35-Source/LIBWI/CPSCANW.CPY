      * COPYMEMBER: LIBWI/CPSCANW
      *******************************************************************
      * REV
      * MJD 001205 ADDED CPSCAN-TYPE: "G" = GLOBAL SCAN
      *                               " " = LOCAL SCAN (DEFAULT)
      *            I ALSO ADDED SOME FILLER FOR FUTURE USE.  (TFC PR#60)
      *  2024.0506 BAH MADE LEVEL 01, CALL NOT WINDOW
      *******************************************************************
       01  CPSCAN-BUF.
           03  CPSCAN-BRNO               PIC 9(4).
           03  CPSCAN-KEY                PIC XXX.
           03  CPSCAN-TYPE               PIC X(01).
           03  CPSCAN-STATUS             PIC XX.
