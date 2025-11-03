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
       SET-LXG-FIELDS.
           MOVE LXG-BRNO TO QLXG-BRNO.
           MOVE LXG-ACCTNO TO QLXG-ACCTNO.
           MOVE LXG-SEQNO TO QLXG-SEQNO.

           IF ( LXG-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLXG-LTOUCH-DATE
           ELSE
              MOVE LXG-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLXG-LTOUCH-DATE.

           MOVE LXG-GLNO(1) TO QLXG-GLNO-1.
           MOVE LXG-GLAMT(1) TO QLXG-GLAMT-1.
           MOVE LXG-GLNO(2) TO QLXG-GLNO-2.
           MOVE LXG-GLAMT(2) TO QLXG-GLAMT-2.
           MOVE LXG-GLNO(3) TO QLXG-GLNO-3.
           MOVE LXG-GLAMT(3) TO QLXG-GLAMT-3.
 

