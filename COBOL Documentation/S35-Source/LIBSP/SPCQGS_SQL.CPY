      * COPYMEMBER: LIBSP/SPCQGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - CQFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD CQ-REC FROM QCQ-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CQ-FIELDS.
           MOVE QCQ-BRNO TO CQ-BRNO.
           MOVE QCQ-ACCTNO TO CQ-ACCTNO.
           MOVE QCQ-COLLID TO CQ-COLLID.
           MOVE QCQ-REV-BEHAVIOR-RISK TO CQ-REV-BEHAVIOR-RISK.
           MOVE QCQ-SEQ-FG TO CQ-SEQ-FG.

           IF ( QCQ-DATE = "1900-01-01" )
              MOVE ZEROES TO CQ-DATE
           ELSE
              MOVE QCQ-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CQ-DATE.

           MOVE QCQ-START-TIME TO CQ-START-TIME.
           MOVE QCQ-REV-DEL TO CQ-REV-DEL.
           MOVE QCQ-MISC-KEY-FLD TO CQ-MISC-KEY-FLD.
           MOVE QCQ-REV-PDUE-DAYS TO CQ-REV-PDUE-DAYS.
           MOVE QCQ-REV-BAL TO CQ-REV-BAL.
           MOVE QCQ-SEQ TO CQ-SEQ.

           IF ( QCQ-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CQ-LTOUCH-DATE
           ELSE
              MOVE QCQ-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CQ-LTOUCH-DATE.

           MOVE QCQ-WORK-COLLID TO CQ-WORK-COLLID.
           MOVE QCQ-WORKED-FG TO CQ-WORKED-FG.
           MOVE QCQ-IN-PROCESS TO CQ-IN-PROCESS.

           IF ( QCQ-DATE-QUEUED = "1900-01-01" )
              MOVE ZEROES TO CQ-DATE-QUEUED
           ELSE
              MOVE QCQ-DATE-QUEUED TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CQ-DATE-QUEUED.

           MOVE QCQ-TIME-ZONE-ERROR TO CQ-TIME-ZONE-ERROR.
 
      *-----------------------------------------------------------------
      *    LOAD QCQ-REC FROM CQ-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CQ-FIELDS.
           MOVE CQ-BRNO TO QCQ-BRNO.
           MOVE CQ-ACCTNO TO QCQ-ACCTNO.
           MOVE CQ-COLLID TO QCQ-COLLID.
           MOVE CQ-REV-BEHAVIOR-RISK TO QCQ-REV-BEHAVIOR-RISK.
           MOVE CQ-SEQ-FG TO QCQ-SEQ-FG.

           IF ( CQ-DATE = ZEROES )
              MOVE "1900-01-01" TO QCQ-DATE
           ELSE
              MOVE CQ-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQ-DATE.

           MOVE CQ-START-TIME TO QCQ-START-TIME.
           MOVE CQ-REV-DEL TO QCQ-REV-DEL.
           MOVE CQ-MISC-KEY-FLD TO QCQ-MISC-KEY-FLD.
           MOVE CQ-REV-PDUE-DAYS TO QCQ-REV-PDUE-DAYS.
           MOVE CQ-REV-BAL TO QCQ-REV-BAL.
           MOVE CQ-SEQ TO QCQ-SEQ.

           IF ( CQ-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCQ-LTOUCH-DATE
           ELSE
              MOVE CQ-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQ-LTOUCH-DATE.

           MOVE CQ-WORK-COLLID TO QCQ-WORK-COLLID.
           MOVE CQ-WORKED-FG TO QCQ-WORKED-FG.
           MOVE CQ-IN-PROCESS TO QCQ-IN-PROCESS.

           IF ( CQ-DATE-QUEUED = ZEROES )
              MOVE "1900-01-01" TO QCQ-DATE-QUEUED
           ELSE
              MOVE CQ-DATE-QUEUED TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQ-DATE-QUEUED.

           MOVE CQ-TIME-ZONE-ERROR TO QCQ-TIME-ZONE-ERROR.
 
