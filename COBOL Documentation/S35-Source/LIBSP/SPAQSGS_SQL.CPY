      * COPYMEMBER: LIBSP/SPAQSGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - AQSFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD AQS-REC FROM QAQS-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-AQS-FIELDS.
           MOVE QAQS-BRNO TO AQS-BRNO.
           MOVE QAQS-NEXT-SEQNO TO AQS-NEXT-SEQNO.
           MOVE QAQS-TOT-REC TO AQS-TOT-REC.
           MOVE QAQS-WORKED-REC TO AQS-WORKED-REC.

           IF ( QAQS-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO AQS-LTOUCH-DATE
           ELSE
              MOVE QAQS-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO AQS-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QAQS-REC FROM AQS-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-AQS-FIELDS.
           MOVE AQS-BRNO TO QAQS-BRNO.
           MOVE AQS-NEXT-SEQNO TO QAQS-NEXT-SEQNO.
           MOVE AQS-TOT-REC TO QAQS-TOT-REC.
           MOVE AQS-WORKED-REC TO QAQS-WORKED-REC.

           IF ( AQS-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QAQS-LTOUCH-DATE
           ELSE
              MOVE AQS-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QAQS-LTOUCH-DATE.

 
