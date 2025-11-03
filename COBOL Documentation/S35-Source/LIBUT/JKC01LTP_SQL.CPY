      * COPYMEMBER: LIBLP/LP01LTP_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN TRAILER FILE - PAYMENT SCHEDULE
      *
      *=================================================================
      * REV:
      * BLM 2021-0504 ***NEW***
      *
      ******************************************************************
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  QLTP1-WHERE-BEG-KEY.
           03  QLTP1-WBEG-BRANCH             PIC 9(04).
           03  QLTP1-WBEG-ACCTNO           PIC 9(08).
           03  QLTP1-WBEG-SEQNO            PIC 9(03).
 
       01  QLTP1-WHERE-END-KEY.
           03  QLTP1-WEND-BRANCH             PIC 9(04).
           03  QLTP1-WEND-ACCTNO           PIC 9(08).
           03  QLTP1-WEND-SEQNO            PIC 9(03).
 
       01  QLTP-REC.
           03  QLTP-BRNO              PIC 9(4).
           03  QLTP-ACCTNO            PIC 9(8).
           03  QLTP-SEQNO             PIC 999.
           03  QLTP-LTOUCH-DATE       PIC X(10).
           03  QLTP-SCHLD-TERM-1     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-1   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-2     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-2   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-3     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-3   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-4     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-4   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-5     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-5   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-6     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-6   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-7     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-7   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-8     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-8   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-9     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-9   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-10    PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-10  PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-11     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-11   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-12     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-12   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-13     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-13   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-14     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-14   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-15     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-15   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-16     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-16   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-17     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-17   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-18     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-18   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-19     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-19   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-20     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-20   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-21     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-21   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-22     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-22   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-23     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-23   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-24     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-24   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-25     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-25   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-26     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-26   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-27     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-27   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-28     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-28   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-29     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-29   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-30     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-30   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-31     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-31   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-32     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-32   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-33     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-33   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-34     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-34   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-35     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-35   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-36     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-36   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-37     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-37   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-38     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-38   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-39     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-39   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-40     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-40   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-41     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-41   PIC 9(6)V99.
       
       EXEC SQL END DECLARE SECTION END-EXEC.
