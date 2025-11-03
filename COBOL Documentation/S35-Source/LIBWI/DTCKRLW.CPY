      * COPYMEMBER: LIBWI/DTCKRLW
      ******************************************************************
      *
      * DTCKRLW
      *
      *=================================================================
      * REV:
      * JTG 010797  ADDED  DTCKRL-NO-TRANS
      *                    DTCKRL-CHECKS
      * BAH 170417 INCREASED DATES TO 9(8) AND LNNO TO 9(8)
      * KEC 2017.0602 INCREASED CLASS FIELDS TO 3 BYTES.
      * BAH 2024.0502 MADE LEVEL 01, CALL NOT WINDOW
      *
      ******************************************************
       01  DTCKRL-BUF.
           03  DTCKRL-BEG-DLNO      PIC 9(4)V99.
           03  DTCKRL-END-DLNO      PIC 9(4)V99.
           03  DTCKRL-BEG-CLASS     PIC 9(3).
           03  DTCKRL-END-CLASS     PIC 9(3).
           03  DTCKRL-BEG-LNNO      PIC 9(8).
           03  DTCKRL-END-LNNO      PIC 9(8).
           03  DTCKRL-BEG-DATE      PIC 9(8).
           03  DTCKRL-END-DATE      PIC 9(8).
           03  DTCKRL-WS-TYPE       PIC X.
           03  DTCKRL-CHECK-TO-DLBR PIC X.
           03  DTCKRL-NEW-DEALS     PIC X.
           03  DTCKRL-WS-BATCH      PIC 9(4).
           03  DTCKRL-BRANCH        PIC 9(4).
           03  DTCKRL-EOF           PIC X.
           03  DTCKRL-LN-CLASS      PIC 9(3).
           03  DTCKRL-LN-DLNO       PIC 9(4)V99.
           03  DTCKRL-NO-TRANS      PIC S9(6).
      * DUPLICATE NAME IN FORM COPY MEMBERS DTCKRL-CHECKS
           03  DTCKRLW-CHECKS       PIC S9(9)V99.
