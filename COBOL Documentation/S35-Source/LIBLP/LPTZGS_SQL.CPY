      * COPYMEMBER: LIBLP/LPTZGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - TZFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD TZ-REC FROM QTZ-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TZ-FIELDS.
           MOVE QTZ-CODE TO TZ-CODE.
           MOVE QTZ-PREFIX TO TZ-PREFIX.
           MOVE QTZ-STATE TO TZ-STATE.
           MOVE QTZ-DESC TO TZ-DESC.
           MOVE QTZ-ZONE TO TZ-ZONE.
           MOVE QTZ-FACTOR TO TZ-FACTOR.
           MOVE QTZ-DLS TO TZ-DLS.

           IF ( QTZ-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TZ-LTOUCH-DATE
           ELSE
              MOVE QTZ-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TZ-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QTZ-REC FROM TZ-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TZ-FIELDS.
           MOVE TZ-CODE TO QTZ-CODE.
           MOVE TZ-PREFIX TO QTZ-PREFIX.
           MOVE TZ-STATE TO QTZ-STATE.
           MOVE TZ-DESC TO QTZ-DESC.
           MOVE TZ-ZONE TO QTZ-ZONE.
           MOVE TZ-FACTOR TO QTZ-FACTOR.
           MOVE TZ-DLS TO QTZ-DLS.

           IF ( TZ-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTZ-LTOUCH-DATE
           ELSE
              MOVE TZ-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTZ-LTOUCH-DATE.

 
