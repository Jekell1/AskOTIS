      * COPYMEMBER: LIBSP/SP01CJ_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - COLLECTOR JOURNAL HISTORY FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  CJ1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  CJ1-CURSOR-STAT-GOOD              VALUE "00".
           88  CJ1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  CJ2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  CJ2-CURSOR-STAT-GOOD              VALUE "00".
           88  CJ2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  CJ2-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  CJ2-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  CJ2-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  CJ1-RETURN-CODE                 PIC S9(4)   COMP-5.

       01  QCJ1-WHERE-BEG-KEY.
           03  QCJ1-WBEG-BRNO              PIC 9(04).
           03  QCJ1-WBEG-COLLID            PIC XXX.
           03  QCJ1-WBEG-DATE              PIC X(10).
           03  QCJ1-WBEG-START-TIME        PIC 9(04).
           03  QCJ1-WBEG-SEQNO             PIC 99.

       01  QCJ1-WHERE-END-KEY.
           03  QCJ1-WEND-BRNO              PIC 9(04).
           03  QCJ1-WEND-COLLID            PIC XXX.
           03  QCJ1-WEND-DATE              PIC X(10).
           03  QCJ1-WEND-START-TIME        PIC 9(04).
           03  QCJ1-WEND-SEQNO             PIC 99.
 
       01  QCJ2-WHERE-BEG-KEY.
           03  QCJ2-WBEG-BRNO              PIC 9(04).
           03  QCJ2-WBEG-ACCTNO            PIC 9(08).
           03  QCJ2-WBEG-DATE              PIC X(10).
           03  QCJ2-WBEG-START-TIME        PIC 9(04).

       01  QCJ2-WHERE-END-KEY.
           03  QCJ2-WEND-BRNO              PIC 9(04).
           03  QCJ2-WEND-ACCTNO            PIC 9(08).
           03  QCJ2-WEND-DATE              PIC X(10).
           03  QCJ2-WEND-START-TIME        PIC 9(04).
 
       01  QCJ1-WHERE-SCAN-BEG-KEY.
           03  QCJ1-WSBEG-COLLID            PIC XXX.
           03  QCJ1-WSBEG-DATE              PIC X(10).
           03  QCJ1-WSBEG-START-TIME        PIC 9(04).
           03  QCJ1-WSBEG-SEQNO             PIC 99.

       01  QCJ-REC.
           03  QCJ-BRNO              PIC 9(4).
           03  QCJ-COLLID            PIC XXX.
           03  QCJ-DATE              PIC X(10).
           03  QCJ-START-TIME        PIC 9(4).
           03  QCJ-SEQNO             PIC 99.
           03  QCJ-LTOUCH-DATE       PIC X(10).
           03  QCJ-ACCTNO            PIC 9(8).
           03  QCJ-END-TIME          PIC 9(4).
           03  QCJ-ACTIVITY-CD       PIC X(3).
           03  QCJ-RESULT-CD         PIC X(3).
           03  QCJ-ACTION-CD         PIC X(4).
           03  QCJ-NEW-COLLID        PIC X(3).
           03  QCJ-CON-STAT          PIC X(3).
           03  QCJ-REC-STAT          PIC X(3).
           03  QCJ-FORM-NO           PIC 9(4).
           03  QCJ-PROMISE-DATE1     PIC X(10).
           03  QCJ-PROMISE-AMT       PIC 9(7)V99.
           03  QCJ-BEG-LM-SEQ        PIC 9(3).
           03  QCJ-END-LM-SEQ        PIC 9(3).
           03  QCJ-PROMISE-DATE2     PIC X(10).
           03  QCJ-CUR-BAL           PIC 9(7)V99.
           03  QCJ-PAST-DUE          PIC 9(7)V99.
           03  QCJ-POSTING-BR        PIC 9(4).
           03  QCJ-QUEUE             PIC X(3).

       EXEC SQL END DECLARE SECTION END-EXEC.
