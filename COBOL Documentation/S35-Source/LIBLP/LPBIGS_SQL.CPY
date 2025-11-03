      * COPYMEMBER: LIBLP/LPBIGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - BIFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0507 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD BI-REC FROM QBI-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-BI-FIELDS.
           MOVE QBI-SSNO TO BI-SSNO.
           MOVE QBI-ACCTNO TO BI-ACCTNO.
           MOVE QBI-OWNBR TO BI-OWNBR.
           MOVE QBI-LNAME TO BI-LNAME.
           MOVE QBI-FNAME TO BI-FNAME.

           IF ( QBI-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO BI-LTOUCH-DATE
           ELSE
              MOVE QBI-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BI-LTOUCH-DATE.

           MOVE QBI-MAKERCD TO BI-MAKERCD.
           MOVE QBI-ORGST TO BI-ORGST.
           MOVE QBI-GIFTNET TO BI-GIFTNET.

           IF ( QBI-POFFDATE = "1900-01-01" )
              MOVE ZEROES TO BI-POFFDATE
           ELSE
              MOVE QBI-POFFDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BI-POFFDATE.


           IF ( QBI-ENTDATE = "1900-01-01" )
              MOVE ZEROES TO BI-ENTDATE
           ELSE
              MOVE QBI-ENTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BI-ENTDATE.

           MOVE QBI-SALEFIN TO BI-SALEFIN.
           MOVE QBI-CONVERCD TO BI-CONVERCD.
           MOVE QBI-PLCD TO BI-PLCD.
           MOVE QBI-SMALLOAN TO BI-SMALLOAN.

           IF ( QBI-PLDATE = "1900-01-01" )
              MOVE ZEROES TO BI-PLDATE
           ELSE
              MOVE QBI-PLDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BI-PLDATE.

 
      *-----------------------------------------------------------------
      *    LOAD QBI-REC FROM BI-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-BI-FIELDS.
           MOVE BI-SSNO TO QBI-SSNO.
           MOVE BI-ACCTNO TO QBI-ACCTNO.
           MOVE BI-OWNBR TO QBI-OWNBR.
           MOVE BI-LNAME TO QBI-LNAME.
           MOVE BI-FNAME TO QBI-FNAME.

           IF ( BI-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QBI-LTOUCH-DATE
           ELSE
              MOVE BI-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBI-LTOUCH-DATE.

           MOVE BI-MAKERCD TO QBI-MAKERCD.
           MOVE BI-ORGST TO QBI-ORGST.
           MOVE BI-GIFTNET TO QBI-GIFTNET.

           IF ( BI-POFFDATE = ZEROES )
              MOVE "1900-01-01" TO QBI-POFFDATE
           ELSE
              MOVE BI-POFFDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBI-POFFDATE.


           IF ( BI-ENTDATE = ZEROES )
              MOVE "1900-01-01" TO QBI-ENTDATE
           ELSE
              MOVE BI-ENTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBI-ENTDATE.

           MOVE BI-SALEFIN TO QBI-SALEFIN.
           MOVE BI-CONVERCD TO QBI-CONVERCD.
           MOVE BI-PLCD TO QBI-PLCD.
           MOVE BI-SMALLOAN TO QBI-SMALLOAN.

           IF ( BI-PLDATE = ZEROES )
              MOVE "1900-01-01" TO QBI-PLDATE
           ELSE
              MOVE BI-PLDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBI-PLDATE.

 
