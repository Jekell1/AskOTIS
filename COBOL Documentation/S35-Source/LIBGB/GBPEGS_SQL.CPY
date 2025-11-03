      * COPYMEMBER: LIBGB/GBPEGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - PEFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD PE-REC FROM QPE-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-PE-FIELDS.
           MOVE QPE-PGM TO PE-PGM.
           MOVE QPE-SEQ TO PE-SEQ.
           MOVE QPE-LOGIN TO PE-LOGIN.

           IF ( QPE-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO PE-LTOUCH-DATE
           ELSE
              MOVE QPE-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PE-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QPE-REC FROM PE-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-PE-FIELDS.
           MOVE PE-PGM TO QPE-PGM.
           MOVE PE-SEQ TO QPE-SEQ.
           MOVE PE-LOGIN TO QPE-LOGIN.

           IF ( PE-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QPE-LTOUCH-DATE
           ELSE
              MOVE PE-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPE-LTOUCH-DATE.

 
