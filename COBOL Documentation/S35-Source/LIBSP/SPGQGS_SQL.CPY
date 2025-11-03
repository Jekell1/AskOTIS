      * COPYMEMBER: LIBSP/SPGQGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - GQFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0507 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD GQ-REC FROM QGQ-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-GQ-FIELDS.
           MOVE QGQ-BRNO TO GQ-BRNO.
           MOVE QGQ-ACCTNO TO GQ-ACCTNO.
           MOVE QGQ-COLLID TO GQ-COLLID.
           MOVE QGQ-SEQ-FG TO GQ-SEQ-FG.

           IF ( QGQ-DATE = "1900-01-01" )
              MOVE ZEROES TO GQ-DATE
           ELSE
              MOVE QGQ-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GQ-DATE.

           MOVE QGQ-START-TIME TO GQ-START-TIME.
           MOVE QGQ-REV-DEL TO GQ-REV-DEL.
           MOVE QGQ-MISC-KEY-FLD TO GQ-MISC-KEY-FLD.
           MOVE QGQ-REV-PDUE-DAYS TO GQ-REV-PDUE-DAYS.
           MOVE QGQ-REV-BAL TO GQ-REV-BAL.
           MOVE QGQ-SEQ TO GQ-SEQ.

           IF ( QGQ-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO GQ-LTOUCH-DATE
           ELSE
              MOVE QGQ-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GQ-LTOUCH-DATE.

           MOVE QGQ-WORK-COLLID TO GQ-WORK-COLLID.
           MOVE QGQ-WORKED-FG TO GQ-WORKED-FG.
           MOVE QGQ-IN-PROCESS TO GQ-IN-PROCESS.

           IF ( QGQ-DATE-QUEUED = "1900-01-01" )
              MOVE ZEROES TO GQ-DATE-QUEUED
           ELSE
              MOVE QGQ-DATE-QUEUED TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GQ-DATE-QUEUED.

           MOVE QGQ-TIME-ZONE-ERROR TO GQ-TIME-ZONE-ERROR.
 
      *-----------------------------------------------------------------
      *    LOAD QGQ-REC FROM GQ-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-GQ-FIELDS.
           MOVE GQ-BRNO TO QGQ-BRNO.
           MOVE GQ-ACCTNO TO QGQ-ACCTNO.
           MOVE GQ-COLLID TO QGQ-COLLID.
           MOVE GQ-SEQ-FG TO QGQ-SEQ-FG.

           IF ( GQ-DATE = ZEROES )
              MOVE "1900-01-01" TO QGQ-DATE
           ELSE
              MOVE GQ-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGQ-DATE.

           MOVE GQ-START-TIME TO QGQ-START-TIME.
           MOVE GQ-REV-DEL TO QGQ-REV-DEL.
           MOVE GQ-MISC-KEY-FLD TO QGQ-MISC-KEY-FLD.
           MOVE GQ-REV-PDUE-DAYS TO QGQ-REV-PDUE-DAYS.
           MOVE GQ-REV-BAL TO QGQ-REV-BAL.
           MOVE GQ-SEQ TO QGQ-SEQ.

           IF ( GQ-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QGQ-LTOUCH-DATE
           ELSE
              MOVE GQ-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGQ-LTOUCH-DATE.

           MOVE GQ-WORK-COLLID TO QGQ-WORK-COLLID.
           MOVE GQ-WORKED-FG TO QGQ-WORKED-FG.
           MOVE GQ-IN-PROCESS TO QGQ-IN-PROCESS.

           IF ( GQ-DATE-QUEUED = ZEROES )
              MOVE "1900-01-01" TO QGQ-DATE-QUEUED
           ELSE
              MOVE GQ-DATE-QUEUED TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGQ-DATE-QUEUED.

           MOVE GQ-TIME-ZONE-ERROR TO QGQ-TIME-ZONE-ERROR.
 
