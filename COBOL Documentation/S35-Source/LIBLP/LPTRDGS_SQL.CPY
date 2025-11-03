      * COPYMEMBER: LIBLP/LPTRDGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - TRDFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD TRD-REC FROM QTRD-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TRD-FIELDS.
           MOVE QTD-BRNO TO TD-BRNO.

           IF ( QTD-DATE = "1900-01-01" )
              MOVE ZEROES TO TD-DATE
           ELSE
              MOVE QTD-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TD-DATE.


           IF ( QTD-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TD-LTOUCH-DATE
           ELSE
              MOVE QTD-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TD-LTOUCH-DATE.

           MOVE QTD-DEPOSIT TO TD-DEPOSIT.
           MOVE QTD-CHECK-DEPOSIT TO TD-CHECK-DEPOSIT.
           MOVE QTD-BANK TO TD-BANK.
           MOVE QTD-CASH TO TD-CASH.
           MOVE QTD-GLFG TO TD-GLFG.
           MOVE QTD-GLOVERSHORT TO TD-GLOVERSHORT.
           MOVE QTD-OVERSHORT TO TD-OVERSHORT.
           MOVE QTD-USERID TO TD-USERID.

           IF ( QTD-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO TD-POSTDATE
           ELSE
              MOVE QTD-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TD-POSTDATE.

           MOVE QTD-POSTTIME TO TD-POSTTIME.
 
      *-----------------------------------------------------------------
      *    LOAD QTRD-REC FROM TRD-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TRD-FIELDS.
           MOVE TD-BRNO TO QTD-BRNO.

           IF ( TD-DATE = ZEROES )
              MOVE "1900-01-01" TO QTD-DATE
           ELSE
              MOVE TD-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTD-DATE.


           IF ( TD-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTD-LTOUCH-DATE
           ELSE
              MOVE TD-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTD-LTOUCH-DATE.

           MOVE TD-DEPOSIT TO QTD-DEPOSIT.
           MOVE TD-CHECK-DEPOSIT TO QTD-CHECK-DEPOSIT.
           MOVE TD-BANK TO QTD-BANK.
           MOVE TD-CASH TO QTD-CASH.
           MOVE TD-GLFG TO QTD-GLFG.
           MOVE TD-GLOVERSHORT TO QTD-GLOVERSHORT.
           MOVE TD-OVERSHORT TO QTD-OVERSHORT.
           MOVE TD-USERID TO QTD-USERID.

           IF ( TD-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QTD-POSTDATE
           ELSE
              MOVE TD-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTD-POSTDATE.

           MOVE TD-POSTTIME TO QTD-POSTTIME.
 
