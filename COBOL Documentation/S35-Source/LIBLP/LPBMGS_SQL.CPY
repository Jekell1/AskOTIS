      * COPYMEMBER: LIBLP/LPBMGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - BMFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD BM-REC FROM QBM-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-BM-FIELDS.
           MOVE QBM-BRNO TO BM-BRNO.
           MOVE QBM-SSNO TO BM-SSNO.

           MOVE QBM-DATE-REVERSED TO BM-DATE-REVERSED.
           MOVE QBM-SEQ TO BM-SEQ.

           IF ( QBM-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO BM-LTOUCH-DATE
           ELSE
              MOVE QBM-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BM-LTOUCH-DATE.

           MOVE QBM-LTOUCH-TIME-HHMM TO BM-LTOUCH-TIME-HHMM.
           MOVE QBM-LTOUCH-USERID TO BM-LTOUCH-USERID.
           MOVE QBM-MEMO TO BM-MEMO.
           MOVE QBM-MEMO-TYPE TO BM-MEMO-TYPE.
 
      *-----------------------------------------------------------------
      *    LOAD QBM-REC FROM BM-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-BM-FIELDS.
           MOVE BM-BRNO TO QBM-BRNO.
           MOVE BM-SSNO TO QBM-SSNO.

           MOVE BM-DATE-REVERSED TO QBM-DATE-REVERSED.
           MOVE BM-SEQ TO QBM-SEQ.

           IF ( BM-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QBM-LTOUCH-DATE
           ELSE
              MOVE BM-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBM-LTOUCH-DATE.

           MOVE BM-LTOUCH-TIME-HHMM TO QBM-LTOUCH-TIME-HHMM.
           MOVE BM-LTOUCH-USERID TO QBM-LTOUCH-USERID.
           MOVE BM-MEMO TO QBM-MEMO.
           MOVE BM-MEMO-TYPE TO QBM-MEMO-TYPE.
 
