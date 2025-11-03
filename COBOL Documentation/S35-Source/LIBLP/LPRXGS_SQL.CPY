      * COPYMEMBER: LIBLP/LPRXGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - RXFILE
      *
      *=================================================================
      * REV:
      * BLM 2021-0511 ***NEW***
      *
      ******************************************************************

      *-----------------------------------------------------------------
      *    LOAD RX-REC FROM QRX-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-RX-FIELDS.
           MOVE QRX-BRNO TO RX-BRNO.
           MOVE QRX-ACCTNO TO RX-ACCTNO.
           MOVE QRX-DLNO TO RX-DLNO.

           IF ( QRX-INTDATE = "1900-01-01" )
              MOVE ZEROES TO RX-INTDATE
           ELSE
              MOVE QRX-INTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RX-INTDATE.

           MOVE QRX-LNAME TO RX-LNAME.
           MOVE QRX-FNAME TO RX-FNAME.

           IF ( QRX-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO RX-LTOUCH-DATE
           ELSE
              MOVE QRX-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RX-LTOUCH-DATE.


           IF ( QRX-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO RX-LOANDATE
           ELSE
              MOVE QRX-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RX-LOANDATE.

           MOVE QRX-LOANAMT TO RX-LOANAMT.
           MOVE QRX-PROCEEDS TO RX-PROCEEDS.
           MOVE QRX-STATUS TO RX-STATUS.
 
      *-----------------------------------------------------------------
      *    LOAD QRX-REC FROM RX-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-RX-FIELDS.
           MOVE RX-BRNO TO QRX-BRNO.
           MOVE RX-ACCTNO TO QRX-ACCTNO.
           MOVE RX-DLNO TO QRX-DLNO.

           IF ( RX-INTDATE = ZEROES )
              MOVE "1900-01-01" TO QRX-INTDATE
           ELSE
              MOVE RX-INTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRX-INTDATE.

           MOVE RX-LNAME TO QRX-LNAME.
           MOVE RX-FNAME TO QRX-FNAME.

           IF ( RX-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QRX-LTOUCH-DATE
           ELSE
              MOVE RX-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRX-LTOUCH-DATE.


           IF ( RX-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QRX-LOANDATE
           ELSE
              MOVE RX-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRX-LOANDATE.

           MOVE RX-LOANAMT TO QRX-LOANAMT.
           MOVE RX-PROCEEDS TO QRX-PROCEEDS.
           MOVE RX-STATUS TO QRX-STATUS.
 
