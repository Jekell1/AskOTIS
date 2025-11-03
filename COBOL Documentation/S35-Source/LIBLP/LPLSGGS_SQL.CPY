      * COPYMEMBER: LIBLP/LPLSGGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - LOAN STATUS - GPS STARTER
      *
      *=================================================================
      * REV:
      * AMM 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD LSG-REC FROM QLSG-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LSG-FIELDS.
           MOVE QLSG-BRNO TO LSG-BRNO.
           MOVE QLSG-ACCTNO TO LSG-ACCTNO.
           MOVE QLSG-SEQNO TO LSG-SEQNO.

           IF ( QLSG-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LSG-LTOUCH-DATE
           ELSE
              MOVE QLSG-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSG-LTOUCH-DATE.

           MOVE QLSG-SERIAL-NO TO LSG-SERIAL-NO.
           MOVE QLSG-STARTER-STAT TO LSG-STARTER-STAT.
           MOVE QLSG-WARNING-STAT TO LSG-WARNING-STAT.
           MOVE QLSG-RECOVERY-STAT TO LSG-RECOVERY-STAT.

           IF ( QLSG-LAST-COMMAND-DATE = "1900-01-01" )
              MOVE ZEROES TO LSG-LAST-COMMAND-DATE
           ELSE
              MOVE QLSG-LAST-COMMAND-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSG-LAST-COMMAND-DATE.

           MOVE QLSG-LAST-ADDRESS TO LSG-LAST-ADDRESS.

           IF ( QLSG-LAST-AUTO-REPORT-DATE = "1900-01-01" )
              MOVE ZEROES TO LSG-LAST-AUTO-REPORT-DATE
           ELSE
              MOVE QLSG-LAST-AUTO-REPORT-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSG-LAST-AUTO-REPORT-DATE.

           MOVE QLSG-GPS-INSTALLER TO LSG-GPS-INSTALLER.
 
      *-----------------------------------------------------------------
      *    LOAD QLSG-REC FROM LSG-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LSG-FIELDS.
           MOVE LSG-BRNO TO QLSG-BRNO.
           MOVE LSG-ACCTNO TO QLSG-ACCTNO.
           MOVE LSG-SEQNO TO QLSG-SEQNO.

           IF ( LSG-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSG-LTOUCH-DATE
           ELSE
              MOVE LSG-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSG-LTOUCH-DATE.

           MOVE LSG-SERIAL-NO TO QLSG-SERIAL-NO.
           MOVE LSG-STARTER-STAT TO QLSG-STARTER-STAT.
           MOVE LSG-WARNING-STAT TO QLSG-WARNING-STAT.
           MOVE LSG-RECOVERY-STAT TO QLSG-RECOVERY-STAT.

           IF ( LSG-LAST-COMMAND-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSG-LAST-COMMAND-DATE
           ELSE
              MOVE LSG-LAST-COMMAND-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSG-LAST-COMMAND-DATE.

           MOVE LSG-LAST-ADDRESS TO QLSG-LAST-ADDRESS.

           IF ( LSG-LAST-AUTO-REPORT-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSG-LAST-AUTO-REPORT-DATE
           ELSE
              MOVE LSG-LAST-AUTO-REPORT-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSG-LAST-AUTO-REPORT-DATE.

           MOVE LSG-GPS-INSTALLER TO QLSG-GPS-INSTALLER.
 

