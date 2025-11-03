      * COPYMEMBER: LIBLP/LPLEGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - LOAN ENDORSER
      *
      *=================================================================
      * REV:
      * AMM 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD LE-REC FROM QLE-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LE-FIELDS.
           MOVE QLE-BRNO TO LE-BRNO.
           MOVE QLE-ACCTNO TO LE-ACCTNO.
           MOVE QLE-TYPE TO LE-TYPE.
           MOVE QLE-FLAG TO LE-FLAG.
           MOVE QLE-SSNO TO LE-SSNO.
           MOVE QLE-OSACCTNO TO LE-OSACCTNO.

           IF ( QLE-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LE-LTOUCH-DATE
           ELSE
              MOVE QLE-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LE-LTOUCH-DATE.

           MOVE QLE-E-OSRATE TO LE-E-OSRATE.
           MOVE QLE-E-OSAMT TO LE-E-OSAMT.

           IF ( QLE-E-OSDATE = "1900-01-01" )
              MOVE ZEROES TO LE-E-OSDATE
           ELSE
              MOVE QLE-E-OSDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LE-E-OSDATE.

           MOVE QLE-E-AMT TO LE-E-AMT.

           IF ( QLE-E-DATE = "1900-01-01" )
              MOVE ZEROES TO LE-E-DATE
           ELSE
              MOVE QLE-E-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LE-E-DATE.

           MOVE QLE-E-OSMAKERCD TO LE-E-OSMAKERCD.

           IF ( QLE-R-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO LE-R-LOANDATE
           ELSE
              MOVE QLE-R-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LE-R-LOANDATE.

           MOVE QLE-R-LNAMT TO LE-R-LNAMT.

           IF ( QLE-R-POFFDATE = "1900-01-01" )
              MOVE ZEROES TO LE-R-POFFDATE
           ELSE
              MOVE QLE-R-POFFDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LE-R-POFFDATE.

           MOVE QLE-R-RESTRICTED-MONEY TO LE-R-RESTRICTED-MONEY.
           MOVE QLE-R-BALANCE TO LE-R-BALANCE.
           MOVE QLE-R-SALEFIN TO LE-R-SALEFIN.
 
      *-----------------------------------------------------------------
      *    LOAD QLE-REC FROM LE-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LE-FIELDS.
           MOVE LE-BRNO TO QLE-BRNO.
           MOVE LE-ACCTNO TO QLE-ACCTNO.
           MOVE LE-TYPE TO QLE-TYPE.
           MOVE LE-FLAG TO QLE-FLAG.
           MOVE LE-SSNO TO QLE-SSNO.
           MOVE LE-OSACCTNO TO QLE-OSACCTNO.

           IF ( LE-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLE-LTOUCH-DATE
           ELSE
              MOVE LE-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLE-LTOUCH-DATE.

           MOVE LE-E-OSRATE TO QLE-E-OSRATE.
           MOVE LE-E-OSAMT TO QLE-E-OSAMT.

           IF ( LE-E-OSDATE = ZEROES )
              MOVE "1900-01-01" TO QLE-E-OSDATE
           ELSE
              MOVE LE-E-OSDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLE-E-OSDATE.

           MOVE LE-E-AMT TO QLE-E-AMT.

           IF ( LE-E-DATE = ZEROES )
              MOVE "1900-01-01" TO QLE-E-DATE
           ELSE
              MOVE LE-E-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLE-E-DATE.

           MOVE LE-E-OSMAKERCD TO QLE-E-OSMAKERCD.

           IF ( LE-R-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QLE-R-LOANDATE
           ELSE
              MOVE LE-R-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLE-R-LOANDATE.

           MOVE LE-R-LNAMT TO QLE-R-LNAMT.

           IF ( LE-R-POFFDATE = ZEROES )
              MOVE "1900-01-01" TO QLE-R-POFFDATE
           ELSE
              MOVE LE-R-POFFDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLE-R-POFFDATE.

           MOVE LE-R-RESTRICTED-MONEY TO QLE-R-RESTRICTED-MONEY.
           MOVE LE-R-BALANCE TO QLE-R-BALANCE.
           MOVE LE-R-SALEFIN TO QLE-R-SALEFIN.
 
