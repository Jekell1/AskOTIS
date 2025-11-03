      * COPYMEMBER: LIBGL/GLGLLOGSGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - GLLOGSFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0507 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD GLLOGS-REC FROM QGLLOGS-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-GLLOGS-FIELDS.
           MOVE QGLLOGS-ID TO GLLOGS-ID.

           IF ( QGLLOGS-SYSTEM-DATE = "1900-01-01" )
              MOVE ZEROES TO GLLOGS-SYSTEM-DATE
           ELSE
              MOVE QGLLOGS-SYSTEM-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GLLOGS-SYSTEM-DATE.

           MOVE QGLLOGS-SEQNO TO GLLOGS-SEQNO.

           IF ( QGLLOGS-BEG-DATE = "1900-01-01" )
              MOVE ZEROES TO GLLOGS-BEG-DATE
           ELSE
              MOVE QGLLOGS-BEG-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GLLOGS-BEG-DATE.


           IF ( QGLLOGS-END-DATE = "1900-01-01" )
              MOVE ZEROES TO GLLOGS-END-DATE
           ELSE
              MOVE QGLLOGS-END-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GLLOGS-END-DATE.

           MOVE QGLLOGS-BRNO-RANGE TO GLLOGS-BRNO-RANGE.
           MOVE QGLLOGS-TIME TO GLLOGS-TIME.
           MOVE QGLLOGS-RECS-PURGED TO GLLOGS-RECS-PURGED.
           MOVE QGLLOGS-USERID TO GLLOGS-USERID.

           IF ( QGLLOGS-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO GLLOGS-LTOUCH-DATE
           ELSE
              MOVE QGLLOGS-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GLLOGS-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QGLLOGS-REC FROM GLLOGS-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-GLLOGS-FIELDS.
           MOVE GLLOGS-ID TO QGLLOGS-ID.

           IF ( GLLOGS-SYSTEM-DATE = ZEROES )
              MOVE "1900-01-01" TO QGLLOGS-SYSTEM-DATE
           ELSE
              MOVE GLLOGS-SYSTEM-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGLLOGS-SYSTEM-DATE.

           MOVE GLLOGS-SEQNO TO QGLLOGS-SEQNO.

           IF ( GLLOGS-BEG-DATE = ZEROES )
              MOVE "1900-01-01" TO QGLLOGS-BEG-DATE
           ELSE
              MOVE GLLOGS-BEG-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGLLOGS-BEG-DATE.


           IF ( GLLOGS-END-DATE = ZEROES )
              MOVE "1900-01-01" TO QGLLOGS-END-DATE
           ELSE
              MOVE GLLOGS-END-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGLLOGS-END-DATE.

           MOVE GLLOGS-BRNO-RANGE TO QGLLOGS-BRNO-RANGE.
           MOVE GLLOGS-TIME TO QGLLOGS-TIME.
           MOVE GLLOGS-RECS-PURGED TO QGLLOGS-RECS-PURGED.
           MOVE GLLOGS-USERID TO QGLLOGS-USERID.

           IF ( GLLOGS-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QGLLOGS-LTOUCH-DATE
           ELSE
              MOVE GLLOGS-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGLLOGS-LTOUCH-DATE.

 
