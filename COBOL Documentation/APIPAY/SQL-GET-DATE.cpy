      ******************************************************************
      *
      *    S Q L - G E T - D A T E
      *
      *=================================================================
      * IN  : SQL-DATE-YYYY-MM-DD
      * OUT : SQL-DATE-YYYYMMDD
      * DESC: CONVERT DATE FROM YYYY-MM-DD TO YYYYMMDD FORMAT;
      *       NEEDED FOR 'GET' FIELDS ROUTINES.
      ******************************************************************
       SQL-GET-DATE.

           IF ( SQL-DATE-YYYY-MM-DD = "1900-01-01" )
              MOVE ZEROES TO SQL-DATE-YYYYMMDD
           ELSE
              INSPECT SQL-DATE-YYYY-MM-DD REPLACING ALL SPACES BY ZEROES
              MOVE SQL-DATE-YYYY-MM-DD-YYYY TO SQL-DATE-YYYYMMDD-YYYY
              MOVE SQL-DATE-YYYY-MM-DD-MM   TO SQL-DATE-YYYYMMDD-MM
              MOVE SQL-DATE-YYYY-MM-DD-DD   TO SQL-DATE-YYYYMMDD-DD.

      ******************************************************************
      *
      *    S Q L - S E T - D A T E
      *
      *=================================================================
      * IN  : SQL-DATE-YYYYMMDD
      * OUT : SQL-DATE-YYYY-MM-DD
      * DESC: CONVERT DATE FROM YYYYMMDD TO YYYY-MM-DD FORMAT;
      *       NEEDED FOR 'SET' FIELDS ROUTINES.
      ******************************************************************
