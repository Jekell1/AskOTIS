      * COPYMEMBER: LIBGB/GBPWGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - PWFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD PW-REC FROM QPW-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-PW-FIELDS.
           MOVE QPW-PGM TO PW-PGM.
           MOVE QPW-SEQ TO PW-SEQ.

           IF ( QPW-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO PW-LTOUCH-DATE
           ELSE
              MOVE QPW-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PW-LTOUCH-DATE.

           MOVE QPW-PASSYN TO PW-PASSYN.
           MOVE QPW-PASSWORD TO PW-PASSWORD.
           MOVE QPW-DESC TO PW-DESC.
           MOVE QPW-USER-PROFILE TO PW-USER-PROFILE.
           MOVE QPW-ASK-FG TO PW-ASK-FG.
           MOVE QPW-EXCEPTION-FG TO PW-EXCEPTION-FG.
 
      *-----------------------------------------------------------------
      *    LOAD QPW-REC FROM PW-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-PW-FIELDS.
           MOVE PW-PGM TO QPW-PGM.
           MOVE PW-SEQ TO QPW-SEQ.

           IF ( PW-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QPW-LTOUCH-DATE
           ELSE
              MOVE PW-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPW-LTOUCH-DATE.

           MOVE PW-PASSYN TO QPW-PASSYN.
           MOVE PW-PASSWORD TO QPW-PASSWORD.
           MOVE PW-DESC TO QPW-DESC.
           MOVE PW-USER-PROFILE TO QPW-USER-PROFILE.
           MOVE PW-ASK-FG TO QPW-ASK-FG.
           MOVE PW-EXCEPTION-FG TO QPW-EXCEPTION-FG.
 
