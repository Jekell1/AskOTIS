      * COPYMEMBER: LIBLP/LPLTDGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - LOAN TRAILER - DEBIT CARD
      *
      *=================================================================
      * REV:
      * AMM 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD LTD-REC FROM QLTD-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LTD-FIELDS.
           MOVE QLTD-BRNO TO LTD-BRNO.
           MOVE QLTD-ACCTNO TO LTD-ACCTNO.
           MOVE QLTD-SEQNO TO LTD-SEQNO.

           IF ( QLTD-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LTD-LTOUCH-DATE
           ELSE
              MOVE QLTD-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTD-LTOUCH-DATE.

           MOVE QLTD-DEBIT-CARD-NUMBER TO LTD-DEBIT-CARD-NUMBER.
           MOVE QLTD-DEBIT-CARD-AMT TO LTD-DEBIT-CARD-AMT.

           IF ( QLTD-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO LTD-LOANDATE
           ELSE
              MOVE QLTD-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTD-LOANDATE.


           IF ( QLTD-VOID-DATE = "1900-01-01" )
              MOVE ZEROES TO LTD-VOID-DATE
           ELSE
              MOVE QLTD-VOID-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTD-VOID-DATE.


           IF ( QLTD-CLEARED-DATE = "1900-01-01" )
              MOVE ZEROES TO LTD-CLEARED-DATE
           ELSE
              MOVE QLTD-CLEARED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTD-CLEARED-DATE.

           MOVE QLTD-CLEARED-AMT TO LTD-CLEARED-AMT.
 
      *-----------------------------------------------------------------
      *    LOAD QLTD-REC FROM LTD-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LTD-FIELDS.
           MOVE LTD-BRNO TO QLTD-BRNO.
           MOVE LTD-ACCTNO TO QLTD-ACCTNO.
           MOVE LTD-SEQNO TO QLTD-SEQNO.

           IF ( LTD-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTD-LTOUCH-DATE
           ELSE
              MOVE LTD-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTD-LTOUCH-DATE.

           MOVE LTD-DEBIT-CARD-NUMBER TO QLTD-DEBIT-CARD-NUMBER.
           MOVE LTD-DEBIT-CARD-AMT TO QLTD-DEBIT-CARD-AMT.

           IF ( LTD-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QLTD-LOANDATE
           ELSE
              MOVE LTD-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTD-LOANDATE.


           IF ( LTD-VOID-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTD-VOID-DATE
           ELSE
              MOVE LTD-VOID-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTD-VOID-DATE.


           IF ( LTD-CLEARED-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTD-CLEARED-DATE
           ELSE
              MOVE LTD-CLEARED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTD-CLEARED-DATE.

           MOVE LTD-CLEARED-AMT TO QLTD-CLEARED-AMT.
 

