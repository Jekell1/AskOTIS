      *================================================================*
      * END COPYBOOK: LIBGL\GLGIGS_SQL.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\GBRCGS_SQL.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGB/GBRCGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - RCFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      * BAH 2025.0410 RC-UPDATED-USERID WAS BEING TESTED AS A DATE S35Q-198
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD RC-REC FROM QRC-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-RC-FIELDS.
           MOVE QRC-STATUS TO RC-STATUS.
           MOVE QRC-BRNO TO RC-BRNO.

           IF ( QRC-TRANS-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-TRANS-DATE
           ELSE
              MOVE QRC-TRANS-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-TRANS-DATE.


           IF ( QRC-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-LTOUCH-DATE
           ELSE
              MOVE QRC-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-LTOUCH-DATE.

           MOVE QRC-RECAP-FG TO RC-RECAP-FG.
           MOVE QRC-GL-FG TO RC-GL-FG.
           MOVE QRC-EOD-BATCH-FG TO RC-EOD-BATCH-FG.
           MOVE QRC-NIGHTLY-FUNDS-FG TO RC-NIGHTLY-FUNDS-FG.
           MOVE QRC-EOM1-BATCH-FG TO RC-EOM1-BATCH-FG.
           MOVE QRC-EOM-UPDATE-FG TO RC-EOM-UPDATE-FG.
           MOVE QRC-EOM2-BATCH-FG TO RC-EOM2-BATCH-FG.
           MOVE QRC-BS-P2P3-INBAL-FG TO RC-BS-P2P3-INBAL-FG.
           MOVE QRC-DEPOSIT TO RC-DEPOSIT.
           MOVE QRC-OVER-SHORT TO RC-OVER-SHORT.
           MOVE QRC-CHECKS TO RC-CHECKS.

           IF ( QRC-OPENED-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-OPENED-DATE
           ELSE
              MOVE QRC-OPENED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-OPENED-DATE.

           MOVE QRC-OPENED-TIME TO RC-OPENED-TIME.
           MOVE QRC-OP-USERID TO RC-OP-USERID.

           IF ( QRC-CLOSED-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-CLOSED-DATE
           ELSE
              MOVE QRC-CLOSED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-CLOSED-DATE.

           MOVE QRC-CLOSED-TIME TO RC-CLOSED-TIME.
           MOVE QRC-CL-USERID TO RC-CL-USERID.

           IF ( QRC-UPDATED-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-UPDATED-DATE
           ELSE
              MOVE QRC-UPDATED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-UPDATED-DATE.

           MOVE QRC-UPDATED-TIME   TO RC-UPDATED-TIME.
           MOVE QRC-UPDATED-USERID TO RC-UPDATED-USERID.

           IF ( QRC-EOD-BATCH-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-EOD-BATCH-DATE
           ELSE
              MOVE QRC-EOD-BATCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-EOD-BATCH-DATE.

           MOVE QRC-EOD-BATCH-TIME TO RC-EOD-BATCH-TIME.

           IF ( QRC-FUNDS-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-FUNDS-DATE
           ELSE
              MOVE QRC-FUNDS-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-FUNDS-DATE.

           MOVE QRC-FUNDS-TIME TO RC-FUNDS-TIME.

           IF ( QRC-EARNINGS-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-EARNINGS-DATE
           ELSE
              MOVE QRC-EARNINGS-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-EARNINGS-DATE.

           MOVE QRC-EARNINGS-TIME TO RC-EARNINGS-TIME.

           IF ( QRC-EOM-UPDATED-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-EOM-UPDATED-DATE
           ELSE
              MOVE QRC-EOM-UPDATED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-EOM-UPDATED-DATE.

           MOVE QRC-EOM-UPDATED-TIME TO RC-EOM-UPDATED-TIME.

           IF ( QRC-EOM2-BATCH-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-EOM2-BATCH-DATE
           ELSE
              MOVE QRC-EOM2-BATCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-EOM2-BATCH-DATE.

           MOVE QRC-EOM2-BATCH-TIME TO RC-EOM2-BATCH-TIME.

           IF ( QRC-CHECK-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-CHECK-DATE
           ELSE
              MOVE QRC-CHECK-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-CHECK-DATE.

           MOVE QRC-CHECK-TIME TO RC-CHECK-TIME.
           MOVE QRC-LAST-ACCRUE TO RC-LAST-ACCRUE.
           MOVE QRC-ERROR-MSG TO RC-ERROR-MSG.
           MOVE QRC-BANK-BAL TO RC-BANK-BAL.
           MOVE QRC-CHECK-DEPOSIT TO RC-CHECK-DEPOSIT.
 
      *-----------------------------------------------------------------
      *    LOAD QRC-REC FROM RC-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
