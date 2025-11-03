      * COPYMEMBER: LIBLP/LP01LXE_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN TRAILER FILE - EARNINGS
      *
      *=================================================================
      * REV:
      * BLM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LXE1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LXE1-CURSOR-STAT-GOOD             VALUE "00".
           88  LXE1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LXE1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLXE1-WHERE-BEG-KEY.
           03  QLXE1-WBEG-BRNO             PIC 9(04).
           03  QLXE1-WBEG-ACCTNO           PIC 9(08).
           03  QLXE1-WBEG-SEQNO            PIC 9(05).

       01  QLXE1-WHERE-END-KEY.
           03  QLXE1-WEND-BRNO             PIC 9(04).
           03  QLXE1-WEND-ACCTNO           PIC 9(08).
           03  QLXE1-WEND-SEQNO            PIC 9(05).

       01  QLXE-REC.
           03  QLXE-BRNO         PIC 9(4).
           03  QLXE-ACCTNO       PIC 9(8).
           03  QLXE-SEQNO        PIC 9(5).
           03  QLXE-LTOUCH-DATE   PIC X(10).
           03  QLXE-EARN-1        PIC S9(7)V99.
           03  QLXE-EARN-2        PIC S9(7)V99.
           03  QLXE-EARN-3        PIC S9(7)V99.
           03  QLXE-EARN-4        PIC S9(7)V99.
           03  QLXE-EARN-5        PIC S9(7)V99.
           03  QLXE-EARN-6        PIC S9(7)V99.
           03  QLXE-EARN-7        PIC S9(7)V99.
           03  QLXE-WORKER        PIC S9(7)V99.
           03  QLXE-WORKER2       PIC S9(7)V99.

       EXEC SQL END DECLARE SECTION END-EXEC.
