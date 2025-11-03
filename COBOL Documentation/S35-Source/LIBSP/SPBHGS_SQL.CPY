      * COPYMEMBER: LIBSP/SPBHGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - BHFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-00507 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD BH-REC FROM QBH-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-BH-FIELDS.
           MOVE QBH-USERID TO BH-USERID.
           MOVE QBH-DATE-REVERSED TO BH-DATE-REVERSED.
           MOVE QBH-TIME TO BH-TIME.
           MOVE QBH-GROUP TO BH-GROUP.
           MOVE QBH-ACTION TO BH-ACTION.

           IF ( QBH-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO BH-LTOUCH-DATE
           ELSE
              MOVE QBH-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BH-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QBH-REC FROM BH-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-BH-FIELDS.
           MOVE BH-USERID TO QBH-USERID.
           MOVE BH-DATE-REVERSED TO QBH-DATE-REVERSED.
           MOVE BH-TIME TO QBH-TIME.
           MOVE BH-GROUP TO QBH-GROUP.
           MOVE BH-ACTION TO QBH-ACTION.

           IF ( BH-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QBH-LTOUCH-DATE
           ELSE
              MOVE BH-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBH-LTOUCH-DATE.

 
