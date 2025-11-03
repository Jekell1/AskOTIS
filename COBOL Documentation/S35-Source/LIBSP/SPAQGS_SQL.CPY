      * COPYMEMBER: LIBSP/SPAQGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - AQFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD AQ-REC FROM QAQ-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-AQ-FIELDS.
           MOVE QAQ-BRNO TO AQ-BRNO.
           MOVE QAQ-ACCTNO TO AQ-ACCTNO.

           IF ( QAQ-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO AQ-LOANDATE
           ELSE
              MOVE QAQ-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO AQ-LOANDATE.

           MOVE QAQ-SEQNO TO AQ-SEQNO.
           MOVE QAQ-WORKED-FG TO AQ-WORKED-FG.

           IF ( QAQ-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO AQ-LTOUCH-DATE
           ELSE
              MOVE QAQ-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO AQ-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QAQ-REC FROM AQ-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-AQ-FIELDS.
           MOVE AQ-BRNO TO QAQ-BRNO.
           MOVE AQ-ACCTNO TO QAQ-ACCTNO.

           IF ( AQ-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QAQ-LOANDATE
           ELSE
              MOVE AQ-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QAQ-LOANDATE.

           MOVE AQ-SEQNO TO QAQ-SEQNO.
           MOVE AQ-WORKED-FG TO QAQ-WORKED-FG.

           IF ( AQ-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QAQ-LTOUCH-DATE
           ELSE
              MOVE AQ-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QAQ-LTOUCH-DATE.

 
