      * COPYMEMBER: LIBLP/LP01LTA_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN TRAILER AUTO INSURANCE FILE
      *
      *=================================================================
      * REV:
      * BLM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LTA1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LTA1-CURSOR-STAT-GOOD             VALUE "00".
           88  LTA1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LTA1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLTA1-WHERE-BEG-KEY.
           03  QLTA1-WBEG-BRNO             PIC 9(04).
           03  QLTA1-WBEG-ACCTNO           PIC 9(08).
           03  QLTA1-WBEG-SEQNO            PIC 9(03).

       01  QLTA1-WHERE-END-KEY.
           03  QLTA1-WEND-BRNO             PIC 9(04).
           03  QLTA1-WEND-ACCTNO           PIC 9(08).
           03  QLTA1-WEND-SEQNO            PIC 9(03).

 
       01  QLTA-REC.
           03  QLTA-BRNO              PIC 9(4).
           03  QLTA-ACCTNO            PIC 9(8).
           03  QLTA-SEQNO             PIC 999.
           03  QLTA-LTOUCH-DATE       PIC X(10).
           03  QLTA-DLR-REQ          PIC X.
           03  QLTA-INS-TYPE         PIC X.
           03  QLTA-INS-CO           PIC X(25).
           03  QLTA-POLICY-NO        PIC X(25).
           03  QLTA-AGENCY           PIC X(25).
           03  QLTA-AGENT            PIC X(25).
           03  QLTA-ADR1             PIC X(25).
           03  QLTA-ADR2             PIC X(25).
           03  QLTA-CITY             PIC X(15).
           03  QLTA-STATE            PIC XX.
           03  QLTA-ZIP              PIC X(10).
           03  QLTA-PHONE            PIC 9(10).
           03  QLTA-EFF-DATE         PIC X(10).
           03  QLTA-EXP-DATE         PIC X(10).
           03  QLTA-CAN-DATE         PIC X(10).
           03  QLTA-CONF-DATE        PIC X(10).
           03  QLTA-LAST-FORM        PIC 9(4).
           03  QLTA-COV-TYPE         PIC X.
           03  QLTA-BIL-COV          PIC S9(7)V99.
           03  QLTA-PDL-COV          PIC S9(7)V99.
           03  QLTA-PIP-COV          PIC S9(7)V99.
           03  QLTA-UMB-COV          PIC S9(7)V99.
           03  QLTA-COL-COV          PIC S9(7)V99.
           03  QLTA-LOSS-PAYEE       PIC X(25).
           03  QLTA-COM-COV          PIC S9(7)V99.
           03  QLTA-TRACK-ONLY       PIC X.
           03  QLTA-INSURANCE        PIC X.
           03  QLTA-TRACK-MONTHS     PIC X(12).
           03  QLTA-INS-MONTHS       PIC X(12).
           03  QLTA-CPP-MONTHS       PIC X(12).

       EXEC SQL END DECLARE SECTION END-EXEC.
