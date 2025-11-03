      * COPYMEMBER: LIBGB/GBSNGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - SNFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD SN-REC FROM QSN-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-SN-FIELDS.
           MOVE QSN-SEQ TO SN-SEQ.
           MOVE QSN-FORM TO SN-FORM.

           IF ( QSN-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO SN-LTOUCH-DATE
           ELSE
              MOVE QSN-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO SN-LTOUCH-DATE.

           MOVE QSN-DESC TO SN-DESC.
           MOVE QSN-PURCHASABLE TO SN-PURCHASABLE.
 
      *-----------------------------------------------------------------
      *    LOAD QSN-REC FROM SN-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-SN-FIELDS.
           MOVE SN-SEQ TO QSN-SEQ.
           MOVE SN-FORM TO QSN-FORM.

           IF ( SN-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QSN-LTOUCH-DATE
           ELSE
              MOVE SN-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QSN-LTOUCH-DATE.

           MOVE SN-DESC TO QSN-DESC.
           MOVE SN-PURCHASABLE TO QSN-PURCHASABLE.
 
