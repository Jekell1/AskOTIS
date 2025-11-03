      * COPYMEMBER: LIBLP/LP01LM_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN MEMO FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      * JKC 2022-0809 ADDED QLM1-WHERE-SCAN-BEG-KEY.
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  LM1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LM1-CURSOR-STAT-GOOD              VALUE "00".
           88  LM1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LM1-CURSOR-LT-STAT           PIC X(2) VALUE SPACES.
           88  LM1-CURSOR-LT-STAT-GOOD           VALUE "00".
           88  LM1-CURSOR-LT-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LM1-CURSOR-SC-STAT           PIC X(2) VALUE SPACES.
           88  LM1-CURSOR-SC-STAT-GOOD           VALUE "00".
           88  LM1-CURSOR-SC-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".


       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LM1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLM1-WHERE-BEG-KEY.
           03  QLM1-WBEG-BRNO             PIC 9(04).
           03  QLM1-WBEG-ACCTNO           PIC 9(08).
           03  QLM1-WBEG-DATE-REV         PIC 9(08).
           03  QLM1-WBEG-SEQ              PIC 9(03).
 
       01  QLM1-WHERE-END-KEY.
           03  QLM1-WEND-BRNO             PIC 9(04).
           03  QLM1-WEND-ACCTNO           PIC 9(08).
           03  QLM1-WEND-DATE-REV         PIC 9(08).
           03  QLM1-WEND-SEQ              PIC 9(03).
 
       01  QLM1-WHERE-SCAN-BEG-KEY.
      *KC  03  QLM1-WSEND-BRNO            PIC 9(04). *NOT NEEDED*
      *KC  03  QLM1-WSBEG-ACCTNO          PIC 9(08). *NOT NEEDED*
           03  QLM1-WSBEG-DATE-REV        PIC 9(08).
           03  QLM1-WSBEG-SEQ             PIC 9(03).

       01  QLM-REC.
           03  QLM-BRNO               PIC 9(4).
           03  QLM-ACCTNO             PIC 9(8).
           03  QLM-DATE-REVERSED      PIC 9(8).
           03  QLM-SEQ                PIC 9(3).
           03  QLM-LTOUCH-DATE            PIC X(10).
           03  QLM-LTOUCH-TIME-HHMM       PIC 9(4).
           03  QLM-LTOUCH-USERID          PIC X(10).
           03  QLM-MEMO                   PIC X(60).
           03  QLM-MEMO-TYPE              PIC XX.

       EXEC SQL END DECLARE SECTION END-EXEC.
