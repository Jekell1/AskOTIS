      * COPYMEMBER: LIBLP/LPLMGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - LOAN MEMO FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD LM-REC FROM QLM-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LM-FIELDS.
           MOVE QLM-BRNO TO LM-BRNO.
           MOVE QLM-ACCTNO TO LM-ACCTNO.

           MOVE QLM-DATE-REVERSED TO LM-DATE-REVERSED.
           MOVE QLM-SEQ TO LM-SEQ.

           IF ( QLM-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LM-LTOUCH-DATE
           ELSE
              MOVE QLM-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LM-LTOUCH-DATE.

           MOVE QLM-LTOUCH-TIME-HHMM TO LM-LTOUCH-TIME-HHMM.
           MOVE QLM-LTOUCH-USERID TO LM-LTOUCH-USERID.
           MOVE QLM-MEMO TO LM-MEMO.
           MOVE QLM-MEMO-TYPE TO LM-MEMO-TYPE.
 
      *-----------------------------------------------------------------
      *    LOAD QLM-REC FROM LM-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LM-FIELDS.
           MOVE LM-BRNO TO QLM-BRNO.
           MOVE LM-ACCTNO TO QLM-ACCTNO.

           MOVE LM-DATE-REVERSED TO QLM-DATE-REVERSED.
           MOVE LM-SEQ TO QLM-SEQ.

           IF ( LM-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLM-LTOUCH-DATE
           ELSE
              MOVE LM-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLM-LTOUCH-DATE.

           MOVE LM-LTOUCH-TIME-HHMM TO QLM-LTOUCH-TIME-HHMM.
           MOVE LM-LTOUCH-USERID TO QLM-LTOUCH-USERID.
           MOVE LM-MEMO TO QLM-MEMO.
           MOVE LM-MEMO-TYPE TO QLM-MEMO-TYPE.
 

