      * COPYMEMBER: LIBSP/SPCJGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - CJFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD CJ-REC FROM QCJ-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CJ-FIELDS.
           MOVE QCJ-BRNO TO CJ-BRNO.
           MOVE QCJ-COLLID TO CJ-COLLID.

           IF ( QCJ-DATE = "1900-01-01" )
              MOVE ZEROES TO CJ-DATE
           ELSE
              MOVE QCJ-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CJ-DATE.

           MOVE QCJ-START-TIME TO CJ-START-TIME.
           MOVE QCJ-SEQNO TO CJ-SEQNO.

           IF ( QCJ-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CJ-LTOUCH-DATE
           ELSE
              MOVE QCJ-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CJ-LTOUCH-DATE.

           MOVE QCJ-ACCTNO TO CJ-ACCTNO.
           MOVE QCJ-END-TIME TO CJ-END-TIME.
           MOVE QCJ-ACTIVITY-CD TO CJ-ACTIVITY-CD.
           MOVE QCJ-RESULT-CD TO CJ-RESULT-CD.
           MOVE QCJ-ACTION-CD TO CJ-ACTION-CD.
           MOVE QCJ-NEW-COLLID TO CJ-NEW-COLLID.
           MOVE QCJ-CON-STAT TO CJ-CON-STAT.
           MOVE QCJ-REC-STAT TO CJ-REC-STAT.
           MOVE QCJ-FORM-NO TO CJ-FORM-NO.

           IF ( QCJ-PROMISE-DATE1 = "1900-01-01" )
              MOVE ZEROES TO CJ-PROMISE-DATE1
           ELSE
              MOVE QCJ-PROMISE-DATE1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CJ-PROMISE-DATE1.

           MOVE QCJ-PROMISE-AMT TO CJ-PROMISE-AMT.
           MOVE QCJ-BEG-LM-SEQ TO CJ-BEG-LM-SEQ.
           MOVE QCJ-END-LM-SEQ TO CJ-END-LM-SEQ.

           IF ( QCJ-PROMISE-DATE2 = "1900-01-01" )
              MOVE ZEROES TO CJ-PROMISE-DATE2
           ELSE
              MOVE QCJ-PROMISE-DATE2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CJ-PROMISE-DATE2.

           MOVE QCJ-CUR-BAL TO CJ-CUR-BAL.
           MOVE QCJ-PAST-DUE TO CJ-PAST-DUE.
           MOVE QCJ-POSTING-BR TO CJ-POSTING-BR.
           MOVE QCJ-QUEUE TO CJ-QUEUE.
 
      *-----------------------------------------------------------------
      *    LOAD QCJ-REC FROM CJ-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CJ-FIELDS.
           MOVE CJ-BRNO TO QCJ-BRNO.
           MOVE CJ-COLLID TO QCJ-COLLID.

           IF ( CJ-DATE = ZEROES )
              MOVE "1900-01-01" TO QCJ-DATE
           ELSE
              MOVE CJ-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCJ-DATE.

           MOVE CJ-START-TIME TO QCJ-START-TIME.
           MOVE CJ-SEQNO TO QCJ-SEQNO.

           IF ( CJ-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCJ-LTOUCH-DATE
           ELSE
              MOVE CJ-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCJ-LTOUCH-DATE.

           MOVE CJ-ACCTNO TO QCJ-ACCTNO.
           MOVE CJ-END-TIME TO QCJ-END-TIME.
           MOVE CJ-ACTIVITY-CD TO QCJ-ACTIVITY-CD.
           MOVE CJ-RESULT-CD TO QCJ-RESULT-CD.
           MOVE CJ-ACTION-CD TO QCJ-ACTION-CD.
           MOVE CJ-NEW-COLLID TO QCJ-NEW-COLLID.
           MOVE CJ-CON-STAT TO QCJ-CON-STAT.
           MOVE CJ-REC-STAT TO QCJ-REC-STAT.
           MOVE CJ-FORM-NO TO QCJ-FORM-NO.

           IF ( CJ-PROMISE-DATE1 = ZEROES )
              MOVE "1900-01-01" TO QCJ-PROMISE-DATE1
           ELSE
              MOVE CJ-PROMISE-DATE1 TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCJ-PROMISE-DATE1.

           MOVE CJ-PROMISE-AMT TO QCJ-PROMISE-AMT.
           MOVE CJ-BEG-LM-SEQ TO QCJ-BEG-LM-SEQ.
           MOVE CJ-END-LM-SEQ TO QCJ-END-LM-SEQ.

           IF ( CJ-PROMISE-DATE2 = ZEROES )
              MOVE "1900-01-01" TO QCJ-PROMISE-DATE2
           ELSE
              MOVE CJ-PROMISE-DATE2 TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCJ-PROMISE-DATE2.

           MOVE CJ-CUR-BAL TO QCJ-CUR-BAL.
           MOVE CJ-PAST-DUE TO QCJ-PAST-DUE.
           MOVE CJ-POSTING-BR TO QCJ-POSTING-BR.
           MOVE CJ-QUEUE TO QCJ-QUEUE.
 
