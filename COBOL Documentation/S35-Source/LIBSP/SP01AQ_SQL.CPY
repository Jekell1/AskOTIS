      * COPYMEMBER: LIBSP/SP01AQ_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - AUDITOR INQUIRY FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  AQ1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  AQ1-CURSOR-STAT-GOOD              VALUE "00".
           88  AQ1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  AQ2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  AQ2-CURSOR-STAT-GOOD              VALUE "00".
           88  AQ2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  AQ1-RETURN-CODE                 PIC S9(4)   COMP-5.
       01  AQ2-RETURN-CODE                 PIC S9(4)   COMP-5.

       01  QAQ1-WHERE-BEG-KEY.
           03  QAQ1-WBEG-BRNO              PIC 9(04).
           03  QAQ1-WBEG-ACCTNO            PIC 9(08).
 
       01  QAQ1-WHERE-END-KEY.
           03  QAQ1-WEND-BRNO              PIC 9(04).
           03  QAQ1-WEND-ACCTNO            PIC 9(08).
 
       01  QAQ2-WHERE-BEG-KEY.
           03  QAQ2-WBEG-BRNO              PIC 9(04).
           03  QAQ2-WBEG-SEQNO             PIC 9(06).
           03  QAQ2-WBEG-LOANDATE          PIC X(10).
           03  QAQ2-WBEG-ACCTNO            PIC 9(08).
 
       01  QAQ1-WHERE-END-KEY.
           03  QAQ2-WEND-BRNO              PIC 9(04).
           03  QAQ2-WEND-SEQNO             PIC 9(06).
           03  QAQ2-WEND-LOANDATE          PIC X(10).
           03  QAQ2-WEND-ACCTNO            PIC 9(08).

       01  QAQ-REC.
           03  QAQ-BRNO               PIC 9(4).
           03  QAQ-ACCTNO             PIC 9(8).
           03  QAQ-LOANDATE           PIC X(10).
           03  QAQ-SEQNO              PIC 9(6).
           03  QAQ-WORKED-FG          PIC X(1).
           03  QAQ-LTOUCH-DATE        PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
