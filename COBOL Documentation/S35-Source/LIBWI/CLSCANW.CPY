      * COPYMEMBER: LIBWI/CLSCANW
      *-------------------------------------------------------------------
      *                     LIBWI/CLSCANW
      * REV:
      *  BAH 082296 CHANGED CLSCAN-BRANCH FROM 999 TO 9(4).
      *  JTG 040212 ALLOWED NON MONTHLY CAN LOANS (NMQUOT.C) WORLD J043
      *  BAH 2024.0416 REMOVED CLSCAN-VERIFY, CLSCAN-GIFTNET
      *  2024.0506 BAH MADE LEVEL 01, CALL NOT WINDOW
      *-------------------------------------------------------------------
       01  CLSCAN-BUF.
           03  CLSCAN-GROUP-ID.
               05  CLSCAN-GROUP-LOC  PIC 9(4).
           03  CLSCAN-SCAN-TYPE      PIC 9    VALUE 1.
           03  CLSCAN-CAN-NO         PIC XX.

      * CLSCAN-NON-MONTHLY = 'Y' IS USED TO SELECT ONLY CAN LOANS
      *    THAT WERE BUILD FROM 'WI/NMQUOT.C'  (NON MONTHLY CAN BUILDER)
      * CLSCAN-NON-MONTHLY = 'A' IS USED TO SELECT ALL CAN LOANS
      *    THAT WERE BUILD FROM 'WI/CLQUOT.C OR WI/NMQUOT.C'
           03  CLSCAN-NON-MONTHLY    PIC X.
 
           03  CLSCAN-STATUS         PIC XX.

