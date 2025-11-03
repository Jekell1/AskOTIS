      * COPYMEMBER: LIBGL/GLGTGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - GTFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0507 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD GT-REC FROM QGT-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-GT-FIELDS.
           MOVE QGT-FLAG TO GT-FLAG.
           MOVE QGT-REF TO GT-REF.

           IF ( QGT-DATE1 = "1900-01-01" )
              MOVE ZEROES TO GT-DATE1
           ELSE
              MOVE QGT-DATE1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GT-DATE1.

           MOVE QGT-SOURCE TO GT-SOURCE.

           IF ( QGT-DATE2 = "1900-01-01" )
              MOVE ZEROES TO GT-DATE2
           ELSE
              MOVE QGT-DATE2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GT-DATE2.

           MOVE QGT-BRANCH TO GT-BRANCH.
           MOVE QGT-ACCTNO TO GT-ACCTNO.

           IF ( QGT-DATE3 = "1900-01-01" )
              MOVE ZEROES TO GT-DATE3
           ELSE
              MOVE QGT-DATE3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GT-DATE3.

           MOVE QGT-DRCRFG TO GT-DRCRFG.
           MOVE QGT-LINE TO GT-LINE.

           IF ( QGT-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO GT-LTOUCH-DATE
           ELSE
              MOVE QGT-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GT-LTOUCH-DATE.

           MOVE QGT-AMT TO GT-AMT.
           MOVE QGT-EXPL TO GT-EXPL.
           MOVE QGT-PERIOD TO GT-PERIOD.
           MOVE QGT-SMITH-SENT TO GT-SMITH-SENT.
           MOVE QGT-POST-USERID TO GT-POST-USERID.

           IF ( QGT-POST-DATE = "1900-01-01" )
              MOVE ZEROES TO GT-POST-DATE
           ELSE
              MOVE QGT-POST-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GT-POST-DATE.

           MOVE QGT-POST-TIME TO GT-POST-TIME.
 
      *-----------------------------------------------------------------
      *    LOAD QGT-REC FROM GT-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-GT-FIELDS.
           MOVE GT-FLAG TO QGT-FLAG.
           MOVE GT-REF TO QGT-REF.

           IF ( GT-DATE1 = ZEROES )
              MOVE "1900-01-01" TO QGT-DATE1
           ELSE
              MOVE GT-DATE1 TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE1.

           MOVE GT-SOURCE TO QGT-SOURCE.

           IF ( GT-DATE2 = ZEROES )
              MOVE "1900-01-01" TO QGT-DATE2
           ELSE
              MOVE GT-DATE2 TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE2.

           MOVE GT-BRANCH TO QGT-BRANCH.
           MOVE GT-ACCTNO TO QGT-ACCTNO.

           IF ( GT-DATE3 = ZEROES )
              MOVE "1900-01-01" TO QGT-DATE3
           ELSE
              MOVE GT-DATE3 TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE3.

           MOVE GT-DRCRFG TO QGT-DRCRFG.
           MOVE GT-LINE TO QGT-LINE.

           IF ( GT-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QGT-LTOUCH-DATE
           ELSE
              MOVE GT-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT-LTOUCH-DATE.

           MOVE GT-AMT TO QGT-AMT.
           MOVE GT-EXPL TO QGT-EXPL.
           MOVE GT-PERIOD TO QGT-PERIOD.
           MOVE GT-SMITH-SENT TO QGT-SMITH-SENT.
           MOVE GT-POST-USERID TO QGT-POST-USERID.

           IF ( GT-POST-DATE = ZEROES )
              MOVE "1900-01-01" TO QGT-POST-DATE
           ELSE
              MOVE GT-POST-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT-POST-DATE.

           MOVE GT-POST-TIME TO QGT-POST-TIME.
 

