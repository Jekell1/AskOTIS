      * COPYMEMBER: LIBSP/SPGQSGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - GQSFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0507 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD GQS-REC FROM QGQS-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-GQS-FIELDS.
           MOVE QGQS-COLLID TO GQS-COLLID.

           IF ( QGQS-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO GQS-LTOUCH-DATE
           ELSE
              MOVE QGQS-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GQS-LTOUCH-DATE.

           MOVE QGQS-TOT-REC TO GQS-TOT-REC.
           MOVE QGQS-WORKED-REC TO GQS-WORKED-REC.
           MOVE QGQS-TIMED-REC TO GQS-TIMED-REC.
           MOVE QGQS-GROUP-ID TO GQS-GROUP-ID.
           MOVE QGQS-GQ-COLLID TO GQS-GQ-COLLID.
           MOVE QGQS-GQ-SEQ-FG TO GQS-GQ-SEQ-FG.

           IF ( QGQS-GQ-DATE = "1900-01-01" )
              MOVE ZEROES TO GQS-GQ-DATE
           ELSE
              MOVE QGQS-GQ-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GQS-GQ-DATE.

           MOVE QGQS-GQ-START-TIME TO GQS-GQ-START-TIME.
           MOVE QGQS-GQ-REV-DEL TO GQS-GQ-REV-DEL.
           MOVE QGQS-GQ-MISC-KEY-FLD TO GQS-GQ-MISC-KEY-FLD.
           MOVE QGQS-GQ-REV-PDUE-DAYS TO GQS-GQ-REV-PDUE-DAYS.
           MOVE QGQS-GQ-REV-BAL TO GQS-GQ-REV-BAL.
           MOVE QGQS-GQ-SEQ TO GQS-GQ-SEQ.
 
      *-----------------------------------------------------------------
      *    LOAD QGQS-REC FROM GQS-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-GQS-FIELDS.
           MOVE GQS-COLLID TO QGQS-COLLID.

           IF ( GQS-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QGQS-LTOUCH-DATE
           ELSE
              MOVE GQS-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGQS-LTOUCH-DATE.

           MOVE GQS-TOT-REC TO QGQS-TOT-REC.
           MOVE GQS-WORKED-REC TO QGQS-WORKED-REC.
           MOVE GQS-TIMED-REC TO QGQS-TIMED-REC.
           MOVE GQS-GROUP-ID TO QGQS-GROUP-ID.
           MOVE GQS-GQ-COLLID TO QGQS-GQ-COLLID.
           MOVE GQS-GQ-SEQ-FG TO QGQS-GQ-SEQ-FG.

           IF ( GQS-GQ-DATE = ZEROES )
              MOVE "1900-01-01" TO QGQS-GQ-DATE
           ELSE
              MOVE GQS-GQ-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGQS-GQ-DATE.

           MOVE GQS-GQ-START-TIME TO QGQS-GQ-START-TIME.
           MOVE GQS-GQ-REV-DEL TO QGQS-GQ-REV-DEL.
           MOVE GQS-GQ-MISC-KEY-FLD TO QGQS-GQ-MISC-KEY-FLD.
           MOVE GQS-GQ-REV-PDUE-DAYS TO QGQS-GQ-REV-PDUE-DAYS.
           MOVE GQS-GQ-REV-BAL TO QGQS-GQ-REV-BAL.
           MOVE GQS-GQ-SEQ TO QGQS-GQ-SEQ.
 
