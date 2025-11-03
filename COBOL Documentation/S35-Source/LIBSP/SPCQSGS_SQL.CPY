      * COPYMEMBER: LIBSP/SPCQSGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - CQSFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD CQS-REC FROM QCQS-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CQS-FIELDS.
           MOVE QCQS-BRNO TO CQS-BRNO.
           MOVE QCQS-COLLID TO CQS-COLLID.

           IF ( QCQS-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CQS-LTOUCH-DATE
           ELSE
              MOVE QCQS-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CQS-LTOUCH-DATE.

           MOVE QCQS-TOT-REC TO CQS-TOT-REC.
           MOVE QCQS-WORKED-REC TO CQS-WORKED-REC.
           MOVE QCQS-TIMED-REC TO CQS-TIMED-REC.
           MOVE QCQS-CQ-BRNO TO CQS-CQ-BRNO.
           MOVE QCQS-CQ-COLLID TO CQS-CQ-COLLID.
           MOVE QCQS-CQ-REV-BEHAVIOR-RISK TO CQS-CQ-REV-BEHAVIOR-RISK.
           MOVE QCQS-CQ-SEQ-FG TO CQS-CQ-SEQ-FG.

           IF ( QCQS-CQ-DATE = "1900-01-01" )
              MOVE ZEROES TO CQS-CQ-DATE
           ELSE
              MOVE QCQS-CQ-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CQS-CQ-DATE.

           MOVE QCQS-CQ-START-TIME TO CQS-CQ-START-TIME.
           MOVE QCQS-CQ-REV-DEL TO CQS-CQ-REV-DEL.
           MOVE QCQS-CQ-MISC-KEY-FLD TO CQS-CQ-MISC-KEY-FLD.
           MOVE QCQS-CQ-REV-PDUE-DAYS TO CQS-CQ-REV-PDUE-DAYS.
           MOVE QCQS-CQ-REV-BAL TO CQS-CQ-REV-BAL.
           MOVE QCQS-CQ-SEQ TO CQS-CQ-SEQ.
 
      *-----------------------------------------------------------------
      *    LOAD QCQS-REC FROM CQS-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CQS-FIELDS.
           MOVE CQS-BRNO TO QCQS-BRNO.
           MOVE CQS-COLLID TO QCQS-COLLID.

           IF ( CQS-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCQS-LTOUCH-DATE
           ELSE
              MOVE CQS-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQS-LTOUCH-DATE.

           MOVE CQS-TOT-REC TO QCQS-TOT-REC.
           MOVE CQS-WORKED-REC TO QCQS-WORKED-REC.
           MOVE CQS-TIMED-REC TO QCQS-TIMED-REC.
           MOVE CQS-CQ-BRNO TO QCQS-CQ-BRNO.
           MOVE CQS-CQ-COLLID TO QCQS-CQ-COLLID.
           MOVE CQS-CQ-REV-BEHAVIOR-RISK TO QCQS-CQ-REV-BEHAVIOR-RISK.
           MOVE CQS-CQ-SEQ-FG TO QCQS-CQ-SEQ-FG.

           IF ( CQS-CQ-DATE = ZEROES )
              MOVE "1900-01-01" TO QCQS-CQ-DATE
           ELSE
              MOVE CQS-CQ-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQS-CQ-DATE.

           MOVE CQS-CQ-START-TIME TO QCQS-CQ-START-TIME.
           MOVE CQS-CQ-REV-DEL TO QCQS-CQ-REV-DEL.
           MOVE CQS-CQ-MISC-KEY-FLD TO QCQS-CQ-MISC-KEY-FLD.
           MOVE CQS-CQ-REV-PDUE-DAYS TO QCQS-CQ-REV-PDUE-DAYS.
           MOVE CQS-CQ-REV-BAL TO QCQS-CQ-REV-BAL.
           MOVE CQS-CQ-SEQ TO QCQS-CQ-SEQ.
 
