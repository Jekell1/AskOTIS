      * COPYMEMBER: LIBLP/LPIXGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - ALTERNATE INDEX
      *
      *=================================================================
      * REV:
      * AMM 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD IX-REC FROM QIX-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-IX-FIELDS.
           MOVE QIX-BRNO TO IX-BRNO.
           MOVE QIX-CODE TO IX-CODE.
           MOVE QIX-ACCTNO TO IX-ACCTNO.
           MOVE QIX-SEQNO TO IX-SEQNO.
           MOVE QIX-VIN TO IX-VIN.

           IF ( QIX-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO IX-LTOUCH-DATE
           ELSE
              MOVE QIX-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO IX-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QIX-REC FROM IX-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-IX-FIELDS.
           MOVE IX-BRNO TO QIX-BRNO.
           MOVE IX-CODE TO QIX-CODE.
           MOVE IX-ACCTNO TO QIX-ACCTNO.
           MOVE IX-SEQNO TO QIX-SEQNO.
           MOVE IX-VIN TO QIX-VIN.

           IF ( IX-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QIX-LTOUCH-DATE
           ELSE
              MOVE IX-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QIX-LTOUCH-DATE.

 

