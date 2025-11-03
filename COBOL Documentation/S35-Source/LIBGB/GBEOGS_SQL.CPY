      * COPYMEMBER: LIBGB/GBEOGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - EOFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0507 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD EO-REC FROM QEO-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-EO-FIELDS.
           MOVE QEO-PCODE TO EO-PCODE.
           MOVE QEO-STEP TO EO-STEP.

           IF ( QEO-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO EO-LTOUCH-DATE
           ELSE
              MOVE QEO-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EO-LTOUCH-DATE.

           MOVE QEO-DIR TO EO-DIR.
           MOVE QEO-PROGRAM TO EO-PROGRAM.
           MOVE QEO-COPIES TO EO-COPIES.
           MOVE QEO-COMMENT TO EO-COMMENT.
           MOVE QEO-MESS TO EO-MESS.

           IF ( QEO-START-DATE = "1900-01-01" )
              MOVE ZEROES TO EO-START-DATE
           ELSE
              MOVE QEO-START-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EO-START-DATE.

           MOVE QEO-START-TIME TO EO-START-TIME.

           IF ( QEO-COMPLETE-DATE = "1900-01-01" )
              MOVE ZEROES TO EO-COMPLETE-DATE
           ELSE
              MOVE QEO-COMPLETE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EO-COMPLETE-DATE.

           MOVE QEO-COMPLETE-TIME TO EO-COMPLETE-TIME.
           MOVE QEO-COMPLETE-STAT TO EO-COMPLETE-STAT.
           MOVE QEO-PARAMETERS TO EO-PARAMETERS.
           MOVE QEO-ERROR-STEP-X TO EO-ERROR-STEP-X.
           MOVE QEO-TRANSFER TO EO-TRANSFER.
           MOVE QEO-LEVEL TO EO-LEVEL.
 
      *-----------------------------------------------------------------
      *    LOAD QEO-REC FROM EO-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-EO-FIELDS.
           MOVE EO-PCODE TO QEO-PCODE.
           MOVE EO-STEP TO QEO-STEP.

           IF ( EO-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QEO-LTOUCH-DATE
           ELSE
              MOVE EO-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEO-LTOUCH-DATE.

           MOVE EO-DIR TO QEO-DIR.
           MOVE EO-PROGRAM TO QEO-PROGRAM.
           MOVE EO-COPIES TO QEO-COPIES.
           MOVE EO-COMMENT TO QEO-COMMENT.
           MOVE EO-MESS TO QEO-MESS.

           IF ( EO-START-DATE = ZEROES )
              MOVE "1900-01-01" TO QEO-START-DATE
           ELSE
              MOVE EO-START-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEO-START-DATE.

           MOVE EO-START-TIME TO QEO-START-TIME.

           IF ( EO-COMPLETE-DATE = ZEROES )
              MOVE "1900-01-01" TO QEO-COMPLETE-DATE
           ELSE
              MOVE EO-COMPLETE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEO-COMPLETE-DATE.

           MOVE EO-COMPLETE-TIME TO QEO-COMPLETE-TIME.
           MOVE EO-COMPLETE-STAT TO QEO-COMPLETE-STAT.
           MOVE EO-PARAMETERS TO QEO-PARAMETERS.
           MOVE EO-ERROR-STEP-X TO QEO-ERROR-STEP-X.
           MOVE EO-TRANSFER TO QEO-TRANSFER.
           MOVE EO-LEVEL TO QEO-LEVEL.
 
