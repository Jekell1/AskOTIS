      * COPYMEMBER: LIBLP/LP01LSR_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN STATUS -  RESCHEDULE/SCRA
      *
      *=================================================================
      * REV:
      * AMM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LSR1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LSR1-CURSOR-STAT-GOOD             VALUE "00".
           88  LSR1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LSR1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLSR1-WHERE-BEG-KEY.
           03  QLSR1-WBEG-BRNO             PIC 9(04).
           03  QLSR1-WBEG-ACCTNO           PIC 9(08).
           03  QLSR1-WBEG-SEQNO            PIC 9(02).

       01  QLSR1-WHERE-END-KEY.
           03  QLSR1-WEND-BRNO             PIC 9(04).
           03  QLSR1-WEND-ACCTNO           PIC 9(08).
           03  QLSR1-WEND-SEQNO            PIC 9(02).

       01  QLSR-REC.
           03  QLSR-BRNO                PIC 9(4).
           03  QLSR-ACCTNO              PIC 9(8).
           03  QLSR-SEQNO               PIC 99.
           03  QLSR-LTOUCH-DATE            PIC X(10).
           03  QLSR-SCRA-ACTIVE            PIC X.
           03  QLSR-SCRA-START-DATE        PIC X(10).
           03  QLSR-SCRA-END-DATE          PIC X(10).
           03  QLSR-SCRA-BORROWER          PIC X.
           03  QLSR-CURRENT-RESCHED-DATE   PIC X(10).
           03  QLSR-CURRENT-REASON-CODE    PIC X(5).
           03  QLSR-INITIAL-RESCHED-DATE   PIC X(10).
           03  QLSR-INITIAL-REASON-CODE    PIC X(5).
           03  QLSR-INITIAL-TDR-DATE       PIC X(10).
           03  QLSR-INITIAL-TDR-REASON     PIC X(5).
           03  QLSR-CURRENT-TDR-DATE       PIC X(10).
           03  QLSR-CURRENT-TDR-REASON     PIC X(5).

       EXEC SQL END DECLARE SECTION END-EXEC.
