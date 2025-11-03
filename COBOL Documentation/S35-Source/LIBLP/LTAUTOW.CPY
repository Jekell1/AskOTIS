      * COPYMEMBER: LIBLP/LTAUTOW
      ******************************************************************
      * REV:
      *  MJD 112697 ENLARGED LT-A-RECORD BY 30 BYTES
      *  BAH 070502 ADDED INSURANCE TRACKING FIELDS IN RECORD3, REGACC PR# 173
      *  BAH 160114 CHANGED TO MATCH NEW LTA-FILE LAYOUT, PD0003
      ******************************************************************
      * MUST MATCH LTA-AUTO-RECORD
       78  WK-AUTO-INS-SIZE          VALUE 302.
       01  WK-AUTO-INS.
           03  WK-A-DLR-REQ          PIC X.
           03  WK-A-INS-TYPE         PIC X.
           03  WK-A-INS-CO           PIC X(25).
           03  WK-A-POLICY-NO        PIC X(25).
           03  WK-A-AGENCY           PIC X(25).
           03  WK-A-AGENT            PIC X(25).

           03  WK-A-ADR1             PIC X(25).
           03  WK-A-ADR2             PIC X(25).
           03  WK-A-CITY             PIC X(15).
           03  WK-A-STATE            PIC XX.
           03  WK-A-ZIP              PIC X(10).
           03  WK-A-PHONE            PIC 9(10)    COMP-3.
           03  WK-A-EFF-DATE         PIC 9(8)     COMP-3.
           03  WK-A-EXP-DATE         PIC 9(8)     COMP-3.
           03  WK-A-CAN-DATE         PIC 9(8)     COMP-3.
           03  WK-A-CONF-DATE        PIC 9(8)     COMP-3.
           03  WK-A-LAST-FORM        PIC 9(4)     COMP-3.

           03  WK-A-COV-TYPE         PIC X.
           03  WK-A-BIL-COV          PIC S9(7)V99 COMP-3.
           03  WK-A-PDL-COV          PIC S9(7)V99 COMP-3.
           03  WK-A-PIP-COV          PIC S9(7)V99 COMP-3.
           03  WK-A-UMB-COV          PIC S9(7)V99 COMP-3.
           03  WK-A-COL-COV          PIC S9(7)V99 COMP-3.
           03  WK-A-LOSS-PAYEE       PIC X(25).
           03  WK-A-COM-COV          PIC S9(7)V99 COMP-3.
           03  WK-A-TRACK-ONLY       PIC X.
           03  WK-A-INSURANCE        PIC X.
           03  WK-A-TRACK-MONTHS     PIC X(12).
           03  WK-A-INS-MONTHS       PIC X(12).
           03  WK-A-CPP-MONTHS       PIC X(12).
