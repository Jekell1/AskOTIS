      * COPYMEMBER: LIBLP/LP01LSJ_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN STATUS -  JUDGEMENT
      *
      *=================================================================
      * REV:
      * AMM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LSJ1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LSJ1-CURSOR-STAT-GOOD             VALUE "00".
           88  LSJ1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LSJ1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLSJ1-WHERE-BEG-KEY.
           03  QLSJ1-WBEG-BRNO             PIC 9(04).
           03  QLSJ1-WBEG-ACCTNO           PIC 9(08).
           03  QLSJ1-WBEG-SEQNO            PIC 9(02).

       01  QLSJ1-WHERE-END-KEY.
           03  QLSJ1-WEND-BRNO             PIC 9(04).
           03  QLSJ1-WEND-ACCTNO           PIC 9(08).
           03  QLSJ1-WEND-SEQNO            PIC 9(02).
 
       01  QLSJ-REC.
           03  QLSJ-BRNO         PIC 9(4).
           03  QLSJ-ACCTNO       PIC 9(8).
           03  QLSJ-SEQNO        PIC 99.
           03  QLSJ-LTOUCH-DATE      PIC X(10).
           03  QLSJ-FILEDATE         PIC X(10).
           03  QLSJ-CASE-NO          PIC X(15).
           03  QLSJ-AMT              PIC S9(7)V99.
           03  QLSJ-COUNTY           PIC X(25).
           03  QLSJ-CLERK-NAME       PIC X(25).
           03  QLSJ-CLERK-ADDR       PIC X(25).
           03  QLSJ-CLERK-CITY       PIC X(15).
           03  QLSJ-CLERK-STATE      PIC X(2).
           03  QLSJ-CLERK-ZIP        PIC X(10).
           03  QLSJ-CLERK-PHONE      PIC 9(10).
           03  QLSJ-SHERIFF-FEE      PIC S9(7)V99.
           03  QLSJ-COURT-FEE        PIC S9(7)V99.
           03  QLSJ-ATTY-FEE         PIC S9(7)V99.
           03  QLSJ-DISMISS-DATE     PIC X(10).
           03  QLSJ-GARN-INFO        PIC X(25).
           03  QLSJ-EMPLOY-NAME      PIC X(25).
           03  QLSJ-EMPLOY-ADDR      PIC X(25).
           03  QLSJ-EMPLOY-CITY      PIC X(15).
           03  QLSJ-EMPLOY-STATE     PIC X(2).
           03  QLSJ-EMPLOY-ZIP       PIC X(10).
           03  QLSJ-GARN-DATE        PIC X(10).
           03  QLSJ-JD-FILED-ON      PIC X.
           03  QLSJ-GARN-FILED-ON    PIC X.
           03  QLSJ-ATTY-NAME        PIC X(25).
           03  QLSJ-ATTY-ADDR        PIC X(25).
           03  QLSJ-ATTY-CITY        PIC X(15).
           03  QLSJ-ATTY-STATE       PIC X(2).
           03  QLSJ-ATTY-ZIP         PIC X(10).
           03  QLSJ-ATTY-PHONE       PIC 9(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
