      *================================================================*
      * END COPYBOOK: LIBLP\LPLXGGS_SQL.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLXEGS_SQL.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLXEGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - LOAN PAYMENT TRAILER - EARNINGS
      *
      *=================================================================
      * REV:
      * AMM 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD LXE-REC FROM QLXE-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LXE-FIELDS.
           MOVE QLXE-BRNO TO LXE-BRNO.
           MOVE QLXE-ACCTNO TO LXE-ACCTNO.
           MOVE QLXE-SEQNO TO LXE-SEQNO.

           IF ( QLXE-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LXE-LTOUCH-DATE
           ELSE
              MOVE QLXE-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LXE-LTOUCH-DATE.

           MOVE QLXE-EARN-1 TO LXE-EARN(1).
           MOVE QLXE-EARN-2 TO LXE-EARN(2).
           MOVE QLXE-EARN-3 TO LXE-EARN(3).
           MOVE QLXE-EARN-4 TO LXE-EARN(4).
           MOVE QLXE-EARN-5 TO LXE-EARN(5).
           MOVE QLXE-EARN-6 TO LXE-EARN(6).
           MOVE QLXE-EARN-7 TO LXE-EARN(7).
           MOVE QLXE-WORKER TO LXE-WORKER.
           MOVE QLXE-WORKER2 TO LXE-WORKER2.
 
      *-----------------------------------------------------------------
      *    LOAD QLXE-REC FROM LXE-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
