      * COPYMEMBER: LIBLP/LP01LTD_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN TRAILER FILE - DEBIT CARD
      *
      *=================================================================
      * REV:
      * BLM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LTD1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LTD1-CURSOR-STAT-GOOD             VALUE "00".
           88  LTD1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LTD1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLTD1-WHERE-BEG-KEY.
           03  QLTD1-WBEG-BRNO             PIC 9(04).
           03  QLTD1-WBEG-ACCTNO           PIC 9(08).
           03  QLTD1-WBEG-SEQNO            PIC 9(03).

       01  QLTD1-WHERE-END-KEY.
           03  QLTD1-WEND-BRNO             PIC 9(04).
           03  QLTD1-WEND-ACCTNO           PIC 9(08).
           03  QLTD1-WEND-SEQNO            PIC 9(03).
 
       01  QLTD-REC.
           03  QLTD-BRNO              PIC 9(4).
           03  QLTD-ACCTNO            PIC 9(8).
           03  QLTD-SEQNO             PIC 999.
           03  QLTD-LTOUCH-DATE           PIC X(10).
           03  QLTD-DEBIT-CARD-NUMBER     PIC 9(10).
           03  QLTD-DEBIT-CARD-AMT        PIC S9(7)V99.
           03  QLTD-LOANDATE              PIC X(10).
           03  QLTD-VOID-DATE             PIC X(10).
           03  QLTD-CLEARED-DATE          PIC X(10).
           03  QLTD-CLEARED-AMT           PIC S9(7)V99.

       EXEC SQL END DECLARE SECTION END-EXEC.
