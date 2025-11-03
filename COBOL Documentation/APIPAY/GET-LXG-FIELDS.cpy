      *================================================================*
      * END COPYBOOK: LIBLP\LPSPGS_SQL.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLXGGS_SQL.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLXGGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - LOAN PAYMENT TRAILER - G/L
      *
      *=================================================================
      * REV:
      * AMM 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD LXG-REC FROM QLXG-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LXG-FIELDS.
           MOVE QLXG-BRNO TO LXG-BRNO.
           MOVE QLXG-ACCTNO TO LXG-ACCTNO.
           MOVE QLXG-SEQNO TO LXG-SEQNO.

           IF ( QLXG-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LXG-LTOUCH-DATE
           ELSE
              MOVE QLXG-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LXG-LTOUCH-DATE.

           MOVE QLXG-GLNO-1 TO LXG-GLNO(1).
           MOVE QLXG-GLAMT-1 TO LXG-GLAMT(1).
           MOVE QLXG-GLNO-2 TO LXG-GLNO(2).
           MOVE QLXG-GLAMT-2 TO LXG-GLAMT(2).
           MOVE QLXG-GLNO-3 TO LXG-GLNO(3).
           MOVE QLXG-GLAMT-3 TO LXG-GLAMT(3).
 
      *-----------------------------------------------------------------
      *    LOAD QLXG-REC FROM LXG-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
