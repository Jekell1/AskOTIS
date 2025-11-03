      * COPYMEMBER: LIBLP/LP01LXG_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN TRAILER FILE - G/L
      *
      *=================================================================
      * REV:
      * BLM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LXG1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LXG1-CURSOR-STAT-GOOD             VALUE "00".
           88  LXG1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LXG1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLXG1-WHERE-BEG-KEY.
           03  QLXG1-WBEG-BRNO             PIC 9(04).
           03  QLXG1-WBEG-ACCTNO           PIC 9(08).
           03  QLXG1-WBEG-SEQNO            PIC 9(05).

       01  QLXG1-WHERE-END-KEY.
           03  QLXG1-WEND-BRNO             PIC 9(04).
           03  QLXG1-WEND-ACCTNO           PIC 9(08).
           03  QLXG1-WEND-SEQNO            PIC 9(05).

       01  QLXG-REC.
           03  QLXG-BRNO         PIC 9(4).
           03  QLXG-ACCTNO       PIC 9(8).
           03  QLXG-SEQNO        PIC 9(5).
           03  QLXG-LTOUCH-DATE      PIC X(10).
           03  QLXG-GLNO-1         PIC 9(11).
           03  QLXG-GLAMT-1        PIC S9(7)V99.
           03  QLXG-GLNO-2         PIC 9(11).
           03  QLXG-GLAMT-2        PIC S9(7)V99.
           03  QLXG-GLNO-3         PIC 9(11).
           03  QLXG-GLAMT-3        PIC S9(7)V99.

       EXEC SQL END DECLARE SECTION END-EXEC.
