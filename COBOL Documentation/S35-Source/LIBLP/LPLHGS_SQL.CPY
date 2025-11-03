      * COPYMEMBER: LIBLP/LPLHGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - LOAN HISTORY FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD LH-REC FROM QLH-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LH-FIELDS.
           MOVE QLH-BRNO TO LH-BRNO.
           MOVE QLH-SSNO TO LH-SSNO.
           MOVE QLH-ACCTNO TO LH-ACCTNO.

           IF ( QLH-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LH-LTOUCH-DATE
           ELSE
              MOVE QLH-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LH-LTOUCH-DATE.

           MOVE QLH-CLASS TO LH-CLASS.
           MOVE QLH-LAWCD TO LH-LAWCD.
           MOVE QLH-COLLCD TO LH-COLLCD.
           MOVE QLH-MAKERCD TO LH-MAKERCD.
           MOVE QLH-POCD TO LH-POCD.

           IF ( QLH-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO LH-LOANDATE
           ELSE
              MOVE QLH-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LH-LOANDATE.


           IF ( QLH-PLDATE = "1900-01-01" )
              MOVE ZEROES TO LH-PLDATE
           ELSE
              MOVE QLH-PLDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LH-PLDATE.


           IF ( QLH-JDDATE = "1900-01-01" )
              MOVE ZEROES TO LH-JDDATE
           ELSE
              MOVE QLH-JDDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LH-JDDATE.


           IF ( QLH-POFFDATE = "1900-01-01" )
              MOVE ZEROES TO LH-POFFDATE
           ELSE
              MOVE QLH-POFFDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LH-POFFDATE.

           MOVE QLH-LNAMT TO LH-LNAMT.
           MOVE QLH-ORGTERM TO LH-ORGTERM.
           MOVE QLH-POTERMS TO LH-POTERMS.
           MOVE QLH-PYPROFILE TO LH-PYPROFILE.
           MOVE QLH-DLNO TO LH-DLNO.
           MOVE QLH-STATUS TO LH-STATUS.
           MOVE QLH-ARCHIVE-FG TO LH-ARCHIVE-FG.
 
      *-----------------------------------------------------------------
      *    LOAD QLH-REC FROM LH-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LH-FIELDS.
           MOVE LH-BRNO TO QLH-BRNO.
           MOVE LH-SSNO TO QLH-SSNO.
           MOVE LH-ACCTNO TO QLH-ACCTNO.

           IF ( LH-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLH-LTOUCH-DATE
           ELSE
              MOVE LH-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLH-LTOUCH-DATE.

           MOVE LH-CLASS TO QLH-CLASS.
           MOVE LH-LAWCD TO QLH-LAWCD.
           MOVE LH-COLLCD TO QLH-COLLCD.
           MOVE LH-MAKERCD TO QLH-MAKERCD.
           MOVE LH-POCD TO QLH-POCD.

           IF ( LH-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QLH-LOANDATE
           ELSE
              MOVE LH-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLH-LOANDATE.


           IF ( LH-PLDATE = ZEROES )
              MOVE "1900-01-01" TO QLH-PLDATE
           ELSE
              MOVE LH-PLDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLH-PLDATE.


           IF ( LH-JDDATE = ZEROES )
              MOVE "1900-01-01" TO QLH-JDDATE
           ELSE
              MOVE LH-JDDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLH-JDDATE.


           IF ( LH-POFFDATE = ZEROES )
              MOVE "1900-01-01" TO QLH-POFFDATE
           ELSE
              MOVE LH-POFFDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLH-POFFDATE.

           MOVE LH-LNAMT TO QLH-LNAMT.
           MOVE LH-ORGTERM TO QLH-ORGTERM.
           MOVE LH-POTERMS TO QLH-POTERMS.
           MOVE LH-PYPROFILE TO QLH-PYPROFILE.
           MOVE LH-DLNO TO QLH-DLNO.
           MOVE LH-STATUS TO QLH-STATUS.
           MOVE LH-ARCHIVE-FG TO QLH-ARCHIVE-FG.
 
