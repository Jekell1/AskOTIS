      * COPYMEMBER: LIBLP/LPFMGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - FMFILE
      *
      *=================================================================
      * REV:
      * BABH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD FM-REC FROM QFM-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-FM-FIELDS.
           MOVE QFM-BRNO TO FM-BRNO.
           MOVE QFM-INITKEY TO FM-INITKEY.
           MOVE QFM-FMNO TO FM-FMNO.
           MOVE QFM-ZIP TO FM-ZIP.
           MOVE QFM-PRINT TO FM-PRINT.
           MOVE QFM-STMT-COUNTER TO FM-STMT-COUNTER.

           IF ( QFM-TRDATE = "1900-01-01" )
              MOVE ZEROES TO FM-TRDATE
           ELSE
              MOVE QFM-TRDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO FM-TRDATE.

           MOVE QFM-USERID TO FM-USERID.

           IF ( QFM-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO FM-LTOUCH-DATE
           ELSE
              MOVE QFM-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO FM-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QFM-REC FROM FM-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-FM-FIELDS.
           MOVE FM-BRNO TO QFM-BRNO.
           MOVE FM-INITKEY TO QFM-INITKEY.
           MOVE FM-FMNO TO QFM-FMNO.
           MOVE FM-ZIP TO QFM-ZIP.
           MOVE FM-PRINT TO QFM-PRINT.
           MOVE FM-STMT-COUNTER TO QFM-STMT-COUNTER.

           IF ( FM-TRDATE = ZEROES )
              MOVE "1900-01-01" TO QFM-TRDATE
           ELSE
              MOVE FM-TRDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QFM-TRDATE.

           MOVE FM-USERID TO QFM-USERID.

           IF ( FM-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QFM-LTOUCH-DATE
           ELSE
              MOVE FM-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QFM-LTOUCH-DATE.

 
