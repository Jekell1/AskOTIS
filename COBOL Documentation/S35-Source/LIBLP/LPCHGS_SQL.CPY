      * COPYMEMBER: LIBOP/LPCHGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - CHFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD CH-REC FROM QCH-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CH-FIELDS.
           MOVE QCH-BRNO TO CH-BRNO.

           IF ( QCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CH-DATE
           ELSE
              MOVE QCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CH-DATE.

           MOVE QCH-CODE TO CH-CODE.
           MOVE QCH-USERID TO CH-USERID.
           MOVE QCH-TIME TO CH-TIME.
           MOVE QCH-SEQ TO CH-SEQ.

           IF ( QCH-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CH-LTOUCH-DATE
           ELSE
              MOVE QCH-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CH-LTOUCH-DATE.

           MOVE QCH-DESCRIPT TO CH-DESCRIPT.
           MOVE QCH-ACCTNO TO CH-ACCTNO.
           MOVE QCH-SSNO TO CH-SSNO.
           MOVE QCH-TYPE TO CH-TYPE.
           MOVE QCH-ORG-CHAR TO CH-ORG-CHAR.
           MOVE QCH-NEW-CHAR TO CH-NEW-CHAR.
 
      *-----------------------------------------------------------------
      *    LOAD QCH-REC FROM CH-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CH-FIELDS.
           MOVE CH-BRNO TO QCH-BRNO.

           IF ( CH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCH-DATE
           ELSE
              MOVE CH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCH-DATE.

           MOVE CH-CODE TO QCH-CODE.
           MOVE CH-USERID TO QCH-USERID.
           MOVE CH-TIME TO QCH-TIME.
           MOVE CH-SEQ TO QCH-SEQ.

           IF ( CH-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCH-LTOUCH-DATE
           ELSE
              MOVE CH-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCH-LTOUCH-DATE.

           MOVE CH-DESCRIPT TO QCH-DESCRIPT.
           MOVE CH-ACCTNO TO QCH-ACCTNO.
           MOVE CH-SSNO TO QCH-SSNO.
           MOVE CH-TYPE TO QCH-TYPE.
           MOVE CH-ORG-CHAR TO QCH-ORG-CHAR.
           MOVE CH-NEW-CHAR TO QCH-NEW-CHAR.
 
