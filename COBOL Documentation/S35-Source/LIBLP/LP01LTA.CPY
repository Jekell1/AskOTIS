      * COPYMEMBER: LIBLP/LP01LTA
      ******************************************************************
      *    AUTO INSURANCE TRAILER FILE 
      *
      *    LTA1-KEY = LTA-BRNO, LTA-ACCTNO, LTA-SEQNO
      *
      ***********************************************************
      * NOTE:
      *   IF YOU CHANGE THIS LAYOUT, YOU MUST CHANGE LP01PC, PC-LTA-RECORD
      ***********************************************************
      * REV:
      *  BAH 07/01/2015 CREATED NEW FROM LP01LT
      *   CS 190506 ADD FUTURE FIELDS
      *  BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20210426 REMOVED FUTURE FIELDS
      *  BAH 20230600 ADDED FILLER
      **************************************************************************
       78  LTA-SIZE                      VALUE 500.
       78  LTA-AUTO-RECORD-SIZE          VALUE 480.
       01  LTA-REC.
           03  LTA1-KEY.
               05  LTA-BRNO              PIC 9(4).
               05  LTA-ACCTNO            PIC 9(8).
               05  LTA-SEQNO             PIC 999.

      $XFD DATE=YYYYMMDD
           03  LTA-LTOUCH-DATE          PIC 9(8) COMP-3.

           03  LTA-AUTO-RECORD.
               05  LTA-DLR-REQ          PIC X.
               05  LTA-INS-TYPE         PIC X.
               05  LTA-INS-CO           PIC X(25).
               05  LTA-POLICY-NO        PIC X(25).
               05  LTA-AGENCY           PIC X(25).
               05  LTA-AGENT            PIC X(25).
               05  LTA-ADR1             PIC X(25).
               05  LTA-ADR2             PIC X(25).
               05  LTA-CITY             PIC X(15).
               05  LTA-STATE            PIC XX.
               05  LTA-ZIP              PIC X(10).
               05  LTA-PHONE            PIC 9(10)    COMP-3.
      $XFD DATE=YYYYMMDD
               05  LTA-EFF-DATE         PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
               05  LTA-EXP-DATE         PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
               05  LTA-CAN-DATE         PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
               05  LTA-CONF-DATE        PIC 9(8)     COMP-3.
               05  LTA-LAST-FORM        PIC 9(4)     COMP-3.
               05  LTA-COV-TYPE         PIC X.
               05  LTA-BIL-COV          PIC S9(7)V99 COMP-3.
               05  LTA-PDL-COV          PIC S9(7)V99 COMP-3.
               05  LTA-PIP-COV          PIC S9(7)V99 COMP-3.
               05  LTA-UMB-COV          PIC S9(7)V99 COMP-3.
               05  LTA-COL-COV          PIC S9(7)V99 COMP-3.
               05  LTA-LOSS-PAYEE       PIC X(25).
               05  LTA-COM-COV          PIC S9(7)V99 COMP-3.
               05  LTA-TRACK-ONLY       PIC X.
               05  LTA-INSURANCE        PIC X.
               05  LTA-TRACK-MONTHS     PIC X(12).
               05  LTA-INS-MONTHS       PIC X(12).
               05  LTA-CPP-MONTHS       PIC X(12).
               05  FILLER               PIC X(178).
