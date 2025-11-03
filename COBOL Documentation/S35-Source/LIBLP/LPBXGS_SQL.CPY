      * COPYMEMBER: LIBLP/LPBXGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - BXFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD BX-REC FROM QBX-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-BX-FIELDS.
           MOVE QBX-BRNO TO BX-BRNO.
           MOVE QBX-SSNO TO BX-SSNO.

           IF ( QBX-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO BX-LTOUCH-DATE
           ELSE
              MOVE QBX-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BX-LTOUCH-DATE.

           MOVE QBX-ALT-ADR1 TO BX-ALT-ADR1.
           MOVE QBX-ALT-ADR2 TO BX-ALT-ADR2.
           MOVE QBX-ALT-CITY TO BX-ALT-CITY.
           MOVE QBX-ALT-STATE TO BX-ALT-STATE.
           MOVE QBX-ALT-ZIP TO BX-ALT-ZIP.
           MOVE QBX-PREV-ADR1 TO BX-PREV-ADR1.
           MOVE QBX-PREV-ADR2 TO BX-PREV-ADR2.
           MOVE QBX-PREV-CITY TO BX-PREV-CITY.
           MOVE QBX-PREV-STATE TO BX-PREV-STATE.
           MOVE QBX-PREV-ZIP TO BX-PREV-ZIP.
           MOVE QBX-KYC-ADR1 TO BX-KYC-ADR1.
           MOVE QBX-KYC-ADR2 TO BX-KYC-ADR2.
           MOVE QBX-KYC-CITY TO BX-KYC-CITY.
           MOVE QBX-KYC-STATE TO BX-KYC-STATE.
           MOVE QBX-KYC-ZIP TO BX-KYC-ZIP.
           MOVE QBX-KYC-IDTYPE TO BX-KYC-IDTYPE.
           MOVE QBX-KYC-ISSAUTH TO BX-KYC-ISSAUTH.
           MOVE QBX-KYC-NUMBER TO BX-KYC-NUMBER.

           IF ( QBX-KYC-ISSDATE = "1900-01-01" )
              MOVE ZEROES TO BX-KYC-ISSDATE
           ELSE
              MOVE QBX-KYC-ISSDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BX-KYC-ISSDATE.


           IF ( QBX-KYC-EXPDATE = "1900-01-01" )
              MOVE ZEROES TO BX-KYC-EXPDATE
           ELSE
              MOVE QBX-KYC-EXPDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BX-KYC-EXPDATE.


           IF ( QBX-KYC-LAST-VERDATE = "1900-01-01" )
              MOVE ZEROES TO BX-KYC-LAST-VERDATE
           ELSE
              MOVE QBX-KYC-LAST-VERDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BX-KYC-LAST-VERDATE.

 
      *-----------------------------------------------------------------
      *    LOAD QBX-REC FROM BX-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-BX-FIELDS.
           MOVE BX-BRNO TO QBX-BRNO.
           MOVE BX-SSNO TO QBX-SSNO.

           IF ( BX-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QBX-LTOUCH-DATE
           ELSE
              MOVE BX-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBX-LTOUCH-DATE.

           MOVE BX-ALT-ADR1 TO QBX-ALT-ADR1.
           MOVE BX-ALT-ADR2 TO QBX-ALT-ADR2.
           MOVE BX-ALT-CITY TO QBX-ALT-CITY.
           MOVE BX-ALT-STATE TO QBX-ALT-STATE.
           MOVE BX-ALT-ZIP TO QBX-ALT-ZIP.
           MOVE BX-PREV-ADR1 TO QBX-PREV-ADR1.
           MOVE BX-PREV-ADR2 TO QBX-PREV-ADR2.
           MOVE BX-PREV-CITY TO QBX-PREV-CITY.
           MOVE BX-PREV-STATE TO QBX-PREV-STATE.
           MOVE BX-PREV-ZIP TO QBX-PREV-ZIP.
           MOVE BX-KYC-ADR1 TO QBX-KYC-ADR1.
           MOVE BX-KYC-ADR2 TO QBX-KYC-ADR2.
           MOVE BX-KYC-CITY TO QBX-KYC-CITY.
           MOVE BX-KYC-STATE TO QBX-KYC-STATE.
           MOVE BX-KYC-ZIP TO QBX-KYC-ZIP.
           MOVE BX-KYC-IDTYPE TO QBX-KYC-IDTYPE.
           MOVE BX-KYC-ISSAUTH TO QBX-KYC-ISSAUTH.
           MOVE BX-KYC-NUMBER TO QBX-KYC-NUMBER.

           IF ( BX-KYC-ISSDATE = ZEROES )
              MOVE "1900-01-01" TO QBX-KYC-ISSDATE
           ELSE
              MOVE BX-KYC-ISSDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBX-KYC-ISSDATE.


           IF ( BX-KYC-EXPDATE = ZEROES )
              MOVE "1900-01-01" TO QBX-KYC-EXPDATE
           ELSE
              MOVE BX-KYC-EXPDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBX-KYC-EXPDATE.


           IF ( BX-KYC-LAST-VERDATE = ZEROES )
              MOVE "1900-01-01" TO QBX-KYC-LAST-VERDATE
           ELSE
              MOVE BX-KYC-LAST-VERDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBX-KYC-LAST-VERDATE.

 
