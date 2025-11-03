      * COPYMEMBER: LIBEX/EX01XCJ
      **************************************************************************
      * COLLECTOR JOURNAL EXTRACTION FILE          218 BYTES
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      * BLV 030207 ADD XCJ-QUEUE, REGACC PR# 0023
      * CS  040903 ADD XCJ-REGION-NUMBER REGACC PR#76
      **************************************************************************
       78  XCJ-SIZE                 VALUE 218.
       01  XCJ-REC.
           03  XCJ-BRACCTNO          PIC 9(10).
           03  XCJ-BRNO              PIC 9(04).
           03  XCJ-COLLID            PIC X(03).
           03  XCJ-DATE              PIC 9(08).
           03  XCJ-START-TIME        PIC 9(04).
           03  XCJ-SEQNO             PIC 9(02).
           03  XCJ-ACCTNO            PIC 9(08).
           03  XCJ-END-TIME          PIC 9(04).
           03  XCJ-ACTIVITY-CD       PIC X(03).
           03  XCJ-RESULT-CD         PIC X(03).
           03  XCJ-ACTION-CD         PIC X(04).
           03  XCJ-NEW-COLLID        PIC X(03).
           03  XCJ-CON-STAT          PIC X(03).
           03  XCJ-REC-STAT          PIC X(03).
           03  XCJ-FORM-NO           PIC 9(03).
           03  XCJ-PROMISE-DATE1     PIC 9(08).
           03  XCJ-PROMISE-AMT       PIC 9(06).99.
           03  XCJ-BEG-LM-SEQ        PIC 9(02).
           03  XCJ-END-LM-SEQ        PIC 9(02).
           03  XCJ-PROMISE-DATE2     PIC 9(08).
           03  XCJ-CUR-BAL           PIC 9(06).99.
           03  XCJ-PAST-DUE          PIC 9(06).99.
           03  XCJ-POSTING-BR        PIC 9(04).
           03  XCJ-QUEUE             PIC X(03).
           03  XCJ-REGION-NUMBER     PIC 9(2).
           03  XCJ-UNUSED            PIC X(95).
           03  XCJ-CR                PIC X(01).
           03  XCJ-LF                PIC X(01).
